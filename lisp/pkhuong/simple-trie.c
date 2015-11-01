#define RUN_ME /*
gcc -W -Wall -O3 -m64 -std=gnu99 $0 -o `basename $0 .c` -lc
exit $?
*/

#include <stdlib.h>
#include <assert.h>
#include <stdint.h>
#include <stdio.h>
#include <strings.h>
#include <string.h>
#include <sys/resource.h>
#include "cycle.h"

// a word ends here
#define STATUS_FULL (1)
#define TAG_MASK (STATUS_FULL)
#define NCHILDREN (26)

typedef union { 
        struct trie_node * ptr;
        uintptr_t bits;
} tagged_trie_node_t;

typedef struct trie_node {
        tagged_trie_node_t tail[NCHILDREN];
} trie_node_t;

typedef struct trie {
        tagged_trie_node_t node;
} trie_t;

#define TAGGED_TRIE_TAG(X) ((X).bits)
#define TAGGED_TRIE_ADDR(X) ((trie_node_t*)((X).bits&~TAG_MASK))
#define TAGGED_TRIE_CHILDREN(X) (TAGGED_TRIE_ADDR(X)->tail)
#define TAGGED_TRIE_LEAF_P(X) ((X).bits <= 1)

#define EMPTY_LEAF 0
#define FULL_LEAF  (STATUS_FULL)

trie_t make_trie ()
{
        trie_t trie = {.node.bits = EMPTY_LEAF};
        return trie;
}

tagged_trie_node_t make_inner_node (int fullp)
{
        tagged_trie_node_t node
                = {.ptr = calloc(sizeof(trie_node_t), 1)};
        TAGGED_TRIE_TAG(node)
                |= (fullp ? STATUS_FULL : 0);
        return node;
}

static inline uint32_t char_idx (char x) { return x - 'a';}

tagged_trie_node_t trie_insert_ (tagged_trie_node_t trie,
                                 const char * string)
{
        if (0 == *string) {
                TAGGED_TRIE_TAG(trie) |= STATUS_FULL;
                return trie;
        }

        if (TAGGED_TRIE_LEAF_P(trie))
                trie = make_inner_node(TAGGED_TRIE_TAG(trie)&STATUS_FULL);

        TAGGED_TRIE_CHILDREN(trie)[char_idx(*string)]
                = trie_insert_(TAGGED_TRIE_CHILDREN(trie)[char_idx(*string)],
                               string+1);
        return trie;
}

void trie_insert (trie_t * trie, const char * string)
{
        trie->node = trie_insert_(trie->node, string);
}

int trie_lookup_ (tagged_trie_node_t trie,
                 const char * string)
{
        if (0 == *string)
                return TAGGED_TRIE_TAG(trie) & STATUS_FULL;

        if (TAGGED_TRIE_LEAF_P(trie))
                return 0;

        return trie_lookup_(TAGGED_TRIE_CHILDREN(trie)[char_idx(*string)],
                            string+1);
}

int trie_lookup (trie_t * trie, const char * string)
{
        return trie_lookup_(trie->node, string);
}

void strtolower (char * str)
{
        while (*str) {
                char c = *str;
                if ((c >= 'A') && (c <= 'Z'))
                        *str += 'a' - 'A';
                str++;
        }
                
}

const char** read_words (const char * file, size_t * OUT_nwords)
{
        FILE * f = fopen(file, "r");
        char buffer[128];
        const char ** words = calloc(sizeof(char *), 16);
        size_t max_words = 16;
        size_t i;
        for (i = 0; 1 == fscanf(f, "%s", buffer); i++) {
                size_t len = strlen(buffer);
                assert(len < 128);
                char * word = malloc(len+1);
                memcpy(word, buffer, len+1);
                strtolower(word);
                while (i >= max_words)
                        words = realloc(words, sizeof(char*)*(max_words*=2));
                words[i] = word;
        }

        *OUT_nwords = i;
        return words;
}

#define RAND(MAX) ((size_t)(random()*(1.0/RAND_MAX)*(MAX)))

void shuffle_vector (const char ** vector, size_t n)
{
        for (size_t i = n; i --> 0;) {
                size_t other = RAND(i+1);
                const char * old = vector[i];
                vector[i] = vector[other];
                vector[other] = old;
        }
}

void insert_words (trie_t * trie, const char ** words, size_t n)
{
        for (size_t i = 0; i < n; i++)
                trie_insert(trie, words[i]);
}

void lookup_words (trie_t * trie, const char ** words, size_t n)
{
        for (size_t i = 0; i < n; i++)
                trie_lookup(trie, words[i]);
}

void lookup_mixed (trie_t * trie, 
                   const char ** words1, size_t n1,
                   const char ** words2, size_t n2)
{
        if (n2 < n1) n1 = n2;

        for (size_t i = 0; i < n1; i++) {
                trie_lookup(trie, words1[i]);
                trie_lookup(trie, words2[i]);
        }
}

#define TIME(NAME, X) do {                                      \
                struct rusage usage;                            \
                assert(0 == getrusage(RUSAGE_SELF, &usage));    \
                double time0 = usage.ru_utime.tv_sec            \
                        + usage.ru_utime.tv_usec*1e-6;          \
                ticks start = getticks();                       \
                X;                                              \
                ticks end = getticks();                         \
                assert(0 == getrusage(RUSAGE_SELF, &usage));    \
                double time1 = usage.ru_utime.tv_sec            \
                        + usage.ru_utime.tv_usec*1e-6;          \
                printf(NAME ": %f (%f)\n",                      \
                       elapsed(end, start), time1-time0);       \
        } while (0)

int main (int argc, char ** argv)
{
        assert(argc > 1);

        size_t nwords = 0;
        const char ** words = read_words(argv[1], &nwords);

        if (argc > 2) {
                shuffle_vector(words, nwords);
                for (size_t i = 0; i < nwords; i++)
                        printf("%s\n", words[i]);
                return 0;
        }

        trie_t trie = make_trie();

        TIME("insert", insert_words(&trie, words, nwords/2));
        TIME("misses", lookup_words(&trie, words+nwords/2, nwords-nwords/2));
        TIME("lookup", lookup_words(&trie, words, nwords/2));
        TIME("mixed ", lookup_mixed(&trie, words, nwords/2,
                                    words+nwords/2, nwords-nwords/2));

        return 0;
}
