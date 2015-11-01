#include "cycle.h"
#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <assert.h>
#include <stdlib.h>

/* preprocessor magic cribbed from cycle.h 
 * N_WORD_BITS is the size of the longest natural word in bits
 */
#ifndef N_WORD_BITS
#  if defined(__x86_64__) || defined(_M_AMD64) || defined(_M_X64)
#    define N_WORD_BITS (64)
#  elif defined(__i386__) || defined(_M_IX86)
#    define N_WORD_BITS (32)
#  else
#    define N_WORD_BITS (32)
#    warning "Assuming N_WORD_BITS is 32. Please define explicitly."
#  endif
#endif

#if (N_WORD_BITS == 64)
#  define WORD uint64_t
#elif (N_WORD_BITS == 32)
#  define WORD uint32_t
#else
#  error "N_WORD_BITS must be 64 or 32."
#endif

#ifndef MAX_INDEX
#  define MAX_INDEX (512)
#endif

#define NMASKS ((MAX_INDEX + N_WORD_BITS - 1)/N_WORD_BITS)

/* Keep the extern around. Helps if you want to compile debug versions
 * or just to see the disasm.
 */
#ifdef HAVE_POPCOUNT_INTRINSIC
extern inline unsigned int
popcount (register WORD x)
{
        return __builtin_popcountl(x);
}
#elif (N_WORD_BITS == 64)
/* c.f. http://www.aggregate.org/MAGIC/#Population%20Count%20(Ones%20Count) */
extern inline unsigned int
popcount (register WORD x)
{
        x -= ((x >> 1) & 0x5555555555555555);
        x = (((x >> 2) & 0x3333333333333333) 
                  + (x & 0x3333333333333333));
        x = (((x >> 4) + x) & 0x0f0f0f0f0f0f0f0f);
        x += (x >> 8);
        x += (x >> 16);
        x += (x >> 32);
        return(x & 0x0000007f);
}
#elif (N_WORD_BITS == 32)
extern inline unsigned int
popcount (register WORD x)
{
        /* 32-bit recursive reduction using SWAR...
	   but first step is mapping 2-bit values
	   into sum of 2 1-bit values in sneaky way
	*/
        x -= ((x >> 1) & 0x55555555);
        x = (((x >> 2) & 0x33333333) + (x & 0x33333333));
        x = (((x >> 4) + x) & 0x0f0f0f0f);
        x += (x >> 8);
        x += (x >> 16);
        return(x & 0x0000003f);
}
#else
#  error "Don't know how to implement popcount here!"
#endif

typedef struct payload_t
{
        uint32_t key;
        int32_t  value;
} payload_t;

typedef struct range_count_t
{
        WORD masks[NMASKS];
        /* partial_sums is initialised and used in bucket_sort.
         * If no one else needs it, and we stick to a hardcoded
         * MAX_INDEX, rip it out.
         */
        uint32_t partial_sums[NMASKS];
        size_t log_count;
        payload_t log[];
} range_count_t;

range_count_t * init_range_count (range_count_t * range)
{
        bzero(range, sizeof(range_count_t));
        return range;
}

range_count_t * make_range_count (size_t log_size)
{
        return init_range_count(malloc(sizeof(range_count_t)+log_size*sizeof(payload_t)));
}

/* Inserting appends payload to the end of the log, linearly.
 * The key'th bit in range->masks is also set, to register the
 * fact that a bucket must be allocated for that key value.
 *
 * Passing the payload by value in an inline function give a tiny
 * bit of help to the compiler, so it can pass the key & value
 * directly, and even write them directly to the log.
 */
extern inline void insert_payload (range_count_t * range, const payload_t payload)
{
        range->log[range->log_count++] = payload;
        int index = payload.key/N_WORD_BITS;
        int bit   = payload.key % N_WORD_BITS;
        range->masks[index] |= 1UL << bit;
}

void test_inserts (range_count_t * range, size_t repeat)
{
        for (size_t i = 0; i < repeat; i++) {
                const payload_t payload = {i%MAX_INDEX, i};
                insert_payload(range, payload);
        }
}

/* Just append to the end of the log, to help estimate the
 * bit-frobbing overhead (tiny).
 */
void test_dummy (range_count_t * range, size_t repeat)
{
        for (size_t i = 0; i < repeat; i++) {
                const payload_t payload = {i%MAX_INDEX, i};
                range->log[range->log_count++] = payload;
        }
}

void test_insertion (range_count_t * range, size_t repeat)
{
        payload_t * log = range->log;
        for (size_t i = 0; i < repeat; i++) {
                const payload_t payload = {i%MAX_INDEX, i};
                for (size_t j = 0; j < range->log_count; j++) {
                        if (log[j].key == payload.key) {
                                log[j].value += payload.value;
                                goto insertion_done;
                        }
                }
                log[range->log_count++] = payload;
        insertion_done:;
        }
}


/* Compute the partial sums for popcount.
 * partial_sums[i] is the number of set bits in masks[0]
 * to masks[i-1]. In other words, it's the number of
 * existent keys < N_WORD_BITS*i.
 */
inline uint32_t compute_partial_sums (range_count_t * range)
{
        uint32_t * partial_sums = range->partial_sums;
        const WORD      * masks = range->masks;
        uint32_t       nbuckets = 0;
        uint32_t counts[NMASKS];

        /* This step can be trivially unrolled
         * and executed dep-free. plz do, kthxbai.
         */
        for (int i = 0; i < NMASKS; i++)
                counts[i] = popcount(masks[i]);
        
        for (int i = 0; i < NMASKS; i++) {
                partial_sums[i] = nbuckets;
                nbuckets += counts[i];
        }

        return nbuckets;
}

/* Initialise buckets, given precomputed partial_sums.
 * It is assumed that there are enough buckets available.
 * We have to fill the key in with the right value, and
 * initialise value to 0.
 *
 * For each mask in masks, while some bits are set, just find
 * the lowest set bit: that's the next bucket. We can compute
 * the corresponding key value by looking at the least set bit's
 * index. Then, zero that bit out to process the next one, if
 * any.
 */
inline void initialise_buckets (payload_t buckets[], const range_count_t * range)
{
        const uint32_t * partial_sums = range->partial_sums;
        const WORD            * masks = range->masks;

        for (unsigned int i = 0, base = 0, bucket = 0;
             i < NMASKS;
             i++, base += N_WORD_BITS) {
                for (WORD mask = masks[i]; mask; bucket++) {
                        int offset = __builtin_ctzl(mask);
                        mask ^= 1UL << offset;
                        buckets[bucket].value = 0;
                        buckets[bucket].key = base+offset;
                }
        }
}

/* Fill the buckets from the log of payload_t.
 * It is assumed that the buckets have already been initialised,
 * and partial_sums computed.
 *
 * Given a key k, we can find the bucket index by counting the
 * number of existing keys smaller than k, or the number of set
 * bit in masks that are less significant than 'k'. partial_sums
 * gives us the number of set bit before the word containing the
 * kth bit, and we count the rest via mask & popcount.
 */
inline void fill_buckets (payload_t buckets[], const range_count_t * range)
{
        const uint32_t  * partial_sums = range->partial_sums;
        const WORD      * masks = range->masks;
        const size_t  log_count = range->log_count;
        const payload_t *   log = range->log;
                
        for (size_t i = 0; i < log_count; i++) {
                uint32_t key  = log[i].key;
                int32_t value = log[i].value;
                int base   = key/N_WORD_BITS;
                WORD key_bit = 1UL << (key%N_WORD_BITS);
                int  index = (partial_sums[base] 
                              + popcount(masks[base] & (key_bit-1)));
                buckets[index].value += value;
        }
}

/* A sparse bucket sort with implicit merging of values.
 * The bitmap range->masks is used to map key values to bucket
 * indices without wasting any space (or density).
 */
int bucket_sort (payload_t buckets[], size_t max_buckets,
                 range_count_t * range)
{
        int nbuckets = compute_partial_sums(range);

        if (nbuckets > max_buckets) return -nbuckets;

        initialise_buckets(buckets, range);
        fill_buckets(buckets, range);
        return nbuckets;
}

int cmp_payload_t (const void* a, const void* b)
{
        const payload_t * x = a;
        const payload_t * y = b;
        return (x->key - y->key) || (x->value - y->value);
}

void test_qsort (payload_t * data, size_t nitems)
{
        qsort(data, nitems, sizeof(payload_t), cmp_payload_t);
}


/* Mise well make sure we're doing it right on this rather
 * simplistic test case.
 */
void sanity_check (range_count_t * range, const size_t ninserts)
{
        size_t counts[MAX_INDEX];
        bzero(counts, MAX_INDEX*sizeof(size_t));

        /* Simulate the run directly */
        for (size_t i = 0; i < ninserts; i++)
                counts[i%MAX_INDEX] += i;

        payload_t data[MAX_INDEX];
        const size_t nbuckets = bucket_sort(data, MAX_INDEX, range);
        if (ninserts < MAX_INDEX)
                assert(ninserts == nbuckets);
        else
                assert(MAX_INDEX == nbuckets);

        int delta_p = 0;
        for (size_t i = 0; i < nbuckets; i++) {
                if ((i != data[i].key) || counts[i] != data[i].value) {
                        printf("%i -> %i; expected %i -> %i\n",
                               data[i].key, data[i].value,
                               i, counts[i]);
                        delta_p = 1;
                }
        }
        assert(!delta_p);
}

int main (int argc, char** argv)
{
        size_t ninserts = 255;
        size_t repetitions = 1000;

        int   test_write = 1;
        int   test_reads = 0;
        int   test_insertion_sort = 0;
        int   test_quicksort = 0;

        if (argc > 1)
                ninserts = atoi(argv[1]);

        if (argc > 2)
                repetitions = atoi(argv[2]);

        if (argc > 3) {
                test_write = !strcmp("writes", argv[3]);
                test_reads = !strcmp("reads", argv[3]);
                test_insertion_sort = !strcmp("insertion", argv[3]);
                test_quicksort = !strcmp("qsort", argv[3]);
        }

        range_count_t * range = make_range_count(ninserts);

        if (test_write) {
                for (size_t i = 0; i < repetitions; i++) {
                        init_range_count(range);
                        ticks beg = getticks();
                        test_inserts(range, ninserts);
                        ticks end = getticks();

                        double time = elapsed(end, beg);
                        printf("inserts: %f %f (%lu)\n", time, time/ninserts, ninserts);
                }
                sanity_check(range, ninserts);
        } else if (test_reads) {
                test_inserts(range, ninserts);
                sanity_check(range, ninserts);
                payload_t *data = malloc(sizeof(payload_t)*MAX_INDEX);
                for (size_t i = 0; i < repetitions; i++) {
                        ticks beg = getticks();
                        bucket_sort(data, MAX_INDEX, range);
                        ticks end = getticks();

                        double time = elapsed(end, beg);
                        printf("sort: %f %f (%lu)\n", time, time/ninserts, ninserts);
                }
        } else if (test_insertion_sort) {
                for (size_t i = 0; i < repetitions; i++) {
                        init_range_count(range);
                        ticks beg = getticks();
                        test_insertion(range, ninserts);
                        ticks end = getticks();

                        double time = elapsed(end, beg);
                        printf("insertion: %f %f (%lu)\n", time, time/ninserts, ninserts);
                }                
        } else if (test_quicksort) {
                test_inserts(range, ninserts);
                payload_t *data = malloc(sizeof(payload_t)*MAX_INDEX);

                for (size_t i = 0; i < repetitions; i++) {
                        memcpy(data, range->log, range->log_count*sizeof(payload_t));
                        ticks beg = getticks();
                        test_qsort(data, range->log_count);
                        ticks end = getticks();

                        double time = elapsed(end, beg);
                        printf("sort: %f %f (%lu)\n", time, time/ninserts, ninserts);
                }
        } else printf("Don't know how to test %s\n", argv[3]);

        return 0;
}
