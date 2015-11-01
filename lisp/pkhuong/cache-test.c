#define RUN_ME_WITH_BASH /*
# Compile with PATTERN = READ(0);                            as cache-test-0;
#                        READ(0); READ(3);                   as cache-test-0-3;
#                        READ(0); READ(3); READ(7);          as cache-test-0-3-7;
#                        READ(0); READ(3); READ(7); READ(8); as cache-test-0-3-7-8

CACHE=;
TLB=;
CYCLES=;

for size in 16 32 128 256 1024 2048 4096 8192 16384 32768 65536 262144 524288 1048576;
do
    CACHE='';
    TLB='';
    CYCLES='';
    for exec in cache-test-0 cache-test-0-3 cache-test-0-3-7 cache-test-0-3-7-8;
    do
        echo "$exec $size" >&2;
        perf stat -e cache-misses,dTLB-load-misses ./$exec 10000000 $size >& record.tmp;
        CACHE="$CACHE	$(grep 'cache-misses' record.tmp | awk '{printf "%6.2f\n", $1/1000000}')";
        TLB="$TLB	$(grep 'dTLB-load-misses' record.tmp | awk '{printf "%6.2f\n", $1/1000000}')";
        CYCLES="$CYCLES	$(head -1 record.tmp | awk '{printf "%6.2f\n", $2}')";
    done;

    for exec in cache-test-0 cache-test-0-3 cache-test-0-3-7 cache-test-0-3-7-8;
    do
        echo "$exec $size TLB" >&2;
        perf stat -e cache-misses,dTLB-load-misses ./$exec 10000000 $size /tmp/hugetlb/foo >& record.tmp;
        CACHE="$CACHE	$(grep 'cache-misses' record.tmp | awk '{printf "%6.2f\n", $1/1000000}')";
        TLB="$TLB	$(grep 'dTLB-load-misses' record.tmp | awk '{printf "%6.2f\n", $1/1000000}')";
        CYCLES="$CYCLES	$(head -1 record.tmp | awk '{printf "%6.2f\n", $2}')";
    done;

    echo "Size: $size";
    echo;
    echo "Cache : $CACHE";
    echo "TLB   : $TLB";
    echo "Cycles: $CYCLES";
    echo;
done;
*/

/*
Total accesses: 160M cache lines

miss (M)
TLB  (M)
cycle

box: 2.8 GHz X5660, DDR3-1333, 32k L1, 256 k L2, 12MB L3
L1 dTLB: 64 * 4k, L1 dTLB huge: 32*2M, L2 TLB: 512 (only small pages)

Access 160M cache lines randomly.
      0: read first word;
    0-3: read first and 4th word;
  0-3-7: read first, 4th and 8th word.
0-3-7-8: first, 4th, 8th and 9th

                +--------------------------------+---------------------------------+
                |           4K pages             :          2M pages               |
                |    0     0-3    0-3-7  0-3-7-8 :   0      0-3    0-3-7   0-3-7-8 |
                +--------------------------------+---------------------------------+
  Size 16K      |                                :                                 |
Cache miss (M)  |   3.89    3.85    3.88    3.88 :   3.58   3.78    3.78    3.79   |
TLB miss   (M)  |   0.50    0.51    0.50    0.58 :   0.16   0.25    0.25    0.25   |
Cycle/pattern   |   4.50    6.25    6.50    6.50 :   4.50   6.25    6.50    6.50   |
                |                                :                                 |
                +--------------------------------+---------------------------------+
  Size 32K      |                                :                                 |
Cache miss (M)  |   3.79    3.87    3.86    3.88 :   3.77   3.78    3.78    3.78   |
TLB miss   (M)  |   0.52    0.51    0.50    0.50 :   0.25   0.24    0.26    0.24   |
Cycle/pattern   |   4.75    6.25    6.25    6.50 :   4.75   6.25    6.25    6.50   |
                |                                :                                 |
                +--------------------------------+---------------------------------+
  Size 128K     |                                :                                 |
Cache miss (M)  |   3.80    3.67    3.84    3.66 :   3.76   3.77    3.77    3.78   |
TLB miss   (M)  |   0.52    0.36    0.50    0.39 :   0.26   0.26    0.24    0.26   |
Cycle/pattern   |   5.25    6.25    6.50    7.25 :   5.25   6.25    6.50    7.25   |
                |                                :                                 |
                +--------------------------------+---------------------------------+
  Size 256K     |                                :                                 |
Cache miss (M)  |   5.03    5.07    5.07    4.06 :   3.98   3.98    3.83    3.83   |
TLB miss   (M)  |   0.51    0.50    0.51    0.47 :   0.27   0.26    0.23    0.25   |
Cycle/pattern   |   5.25    6.50    7.25    7.25 :   5.25   6.25    6.75    7.25   |
                |                                :                                 |
                +--------------------------------+---------------------------------+


                +--------------------------------+---------------------------------+
                |           4K pages             :          2M pages               |
                |    0     0-3    0-3-7  0-3-7-8 :   0      0-3    0-3-7   0-3-7-8 |
                +--------------------------------+---------------------------------+
  Size 1M       |                                :                                 |
Cache miss (M)  |   5.04    5.09    5.09    5.09 :   5.00   4.99    5.00    4.99   |
TLB miss   (M)  |   0.50    0.50    0.49    0.50 :   0.23   0.25    0.24    0.24   |
Cycle/pattern   |   6.25    7.25    8.50   10.50 :   5.25   6.75    7.75    9.50   |
                |                                :                                 |
                +--------------------------------+---------------------------------+
  Size 2M       |                                :                                 |
Cache miss (M)  |   5.06    5.10    5.11    5.14 :   5.03   5.01    5.02    5.05   |
TLB miss   (M)  |   0.67    0.85    0.88    0.90 :   0.23   0.27    0.23    0.24   |
Cycle/pattern   |   6.50    7.25    8.75   10.75 :   5.25   6.75    7.75    9.75   |
                |                                :                                 |
                +--------------------------------+---------------------------------+
  Size 4M       |                                :                                 |
Cache miss (M)  |   5.19    5.19    5.19    5.22 :   5.08   5.07    5.08    5.11   |
TLB miss   (M)  |  80.42   80.59   80.70   81.98 :   0.24   0.25    0.24    0.24   |
Cycle/pattern   |   8.25   10.00   12.00   13.75 :   5.00   6.75    7.75    9.75   |
                |                                :                                 |
                +--------------------------------+---------------------------------+
  Size 8M       |                                :                                 |
Cache miss (M)  |   7.29    7.28    8.67    8.68 :   5.21   5.22    5.22    5.25   |
TLB miss   (M)  | 120.45  120.55  120.63  122.52 :   0.23   0.30    0.27    0.25   |
Cycle/pattern   |  11.00   13.00   15.50   17.25 :   5.00   6.75    7.75   10.00   |
                |                                :                                 |
                +--------------------------------+---------------------------------+


                +--------------------------------+---------------------------------+
                |           4K pages             :          2M pages               |
                |    0     0-3    0-3-7  0-3-7-8 :   0      0-3    0-3-7   0-3-7-8 |
                +--------------------------------+---------------------------------+
  Size 16M      |                                :                                 |
Cache miss (M)  |  49.37   49.41   90.72   91.77 :  47.82  47.72   81.46   88.01   |
TLB miss   (M)  | 140.59  140.60  140.67  142.87 :   0.25   0.25    0.24    0.25   |
Cycle/pattern   |  18.00   20.50   24.00   26.50 :  14.00  15.25   17.00   18.75   |
                |                                :                                 |
                +--------------------------------+---------------------------------+
  Size 32M      |                                :                                 |
Cache miss (M)  | 106.93  107.09  203.73  207.82 : 106.55  106.62  186.50  206.83  |
TLB miss   (M)  | 150.56  150.74  150.82  153.09 :   0.24   0.26    0.25    0.27   |
Cycle/pattern   |  22.25   24.75   31.00   34.00 :  15.75  17.25   20.00   27.50   |
                |                                :                                 |
                +--------------------------------+---------------------------------+
  Size 64M      |                                :                                 |
Cache miss (M)  | 137.03  137.23  263.73  267.88 : 136.67  136.82  232.64  266.93  |
TLB miss   (M)  | 155.63  155.79  155.81  158.21 :   5.09   5.25    5.69    5.78   |
Cycle/pattern   |  26.00   29.25   36.75   39.75 :  16.75  18.25   24.25   30.75   |
                |                                :                                 |
                +--------------------------------+---------------------------------+


                +--------------------------------+---------------------------------+
                |           4K pages             :          2M pages               |
                |    0     0-3    0-3-7  0-3-7-8 :   0      0-3    0-3-7   0-3-7-8 |
                +--------------------------------+---------------------------------+
  Size 128M     |                                :                                 |
Cache miss (M)  | 153.21  153.35  296.01  299.06 : 152.77  152.86  261.09  298.71  |
TLB miss   (M)  | 158.00  158.07  158.10  160.58 :  80.84   84.96   91.48   96.40  |
Cycle/pattern   |  30.50   34.50   41.00   44.00 :  18.75   20.75   27.25   33.25  |
                |                                :                                 |
                +--------------------------------+---------------------------------+
  Size 512M     |                                :                                 |
Cache miss (M)  | 170.65  170.90  326.84  329.59 : 169.90  170.22  286.47  326.54  |
TLB miss   (M)  | 160.39  160.41  162.17  164.62 : 140.35  147.28  160.81  179.58  |
Cycles/patterm  |  36.75   41.00   47.00   50.00 :  20.50  23.00   29.75   35.50   |
                |                                :                                 |
                +--------------------------------+---------------------------------+
  Size 1G       |                                :                                 |
Cache miss (M)  | 184.29  184.29  353.23  356.73 : 180.11  180.43  300.66  338.62  |
TLB miss   (M)  | 163.64  164.26  176.04  178.88 : 150.37  157.85  169.58  190.89  |
Cycle/pattern   |  37.25   41.50   52.00   55.25 :  22.00  24.75   30.50   37.00   |
                |                                :                                 |
                +--------------------------------+---------------------------------+
 */
#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <string.h>
#include <strings.h>
#include <sys/mman.h>
#include <fcntl.h>
#include <unistd.h>
#include "cycle.h"

typedef unsigned long u64;
typedef unsigned u32;

struct cache_line {
        u64 word[8];
};

u64 page_round (u64 n)
{
        n++;
        u64 page = 2*1024*1024;
        return (n+page-1)&(-page);
}

const char * path = 0;

void * alloc (u64 size)
{
        size = page_round(size);
        void * addr = MAP_FAILED;
        int fd = 0;
        if (!path) {
                addr = mmap(NULL, size,
                            PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANON,
                            0, 0);
        } else {
                fd = open(path, O_RDONLY);
                if (fd == -1) {
                        perror("open");
                        exit(1);
                }

                addr = mmap(NULL, size,
                            PROT_READ | PROT_WRITE, MAP_PRIVATE,
                            fd, 0);
        }

        if (MAP_FAILED == addr) {
                perror("mmap");
                exit(1);
        }

        if (path)
                close(fd);

        memset(addr, 0, size);
        return addr;
}

static
int cmp_double (const void * a, const void * b)
{
        double x = *(double *)a;
        double y = *(double *)b;

        if (x == y) return 0;
        if (x < y) return -1;
        return 1;
}

void print_histogram (double * times, size_t n)
{
        qsort(times, n, sizeof(double), cmp_double);

        double  min = times[(size_t)round(n*.001)],
                max = times[(size_t)round(n*.99)];

        printf("%f %f %f\n", min, times[n/2], max);

#if 0
        size_t counts[22];
        double limits[22];
        for (size_t i = 0; i < 21; i++)
                limits[i] = min+(i*((max-min)/20));
        limits[21] = times[n-1];

        bzero(counts, sizeof(size_t)*22);

        {
                size_t index = 0;
                double limit = limits[index];
                for (size_t i = 0; i < n; i++) {
                        double time = times[i];
                        if (time <= limit+1e-5) {
                                counts[index]++;
                        } else {
                                while (time > limit+1e-5) {
                                        limit = limits[++index];
                                        assert(index < 22);
                                }
                        }
                }
        }

        size_t max_count = 0;
        for (size_t i = 0; i < 22; i++) {
                if (max_count < counts[i])
                        max_count = counts[i];
        }

        printf("min: %4.0f\n", min);
        double star_per_count = 100.0/n;
        size_t max_stars = (size_t)(max_count*star_per_count)+1;
        for (size_t i = 0; i < 22; i++) {
                printf("<= %4.0f: ", limits[i]);
                size_t stars = (size_t)ceil(star_per_count*counts[i]);
                for (size_t j = 0; j < stars; j++)
                        putchar('*');
                for (size_t j = stars; j <= max_stars; j++)
                        putchar(' ');
                printf("\t%2.1f\n", counts[i]*star_per_count);
        }
#endif
}

#define SUBLOOP 16

#ifndef PATTERN
//#define PATTERN READ(0)
//#define PATTERN READ(0); READ(3);
//#define PATTERN READ(0); READ(3); READ(7);
#define PATTERN READ(0); READ(3); READ(7); READ(8);
#endif

u64 bench (u32 n, u32 mask, double * times, struct cache_line * data)
{
        u64 acc = 0;
        unsigned lfsr = 1;
        for (size_t i = 0; i < n; i++) {
                ticks start = getticks();
                for (size_t j = 0; j < SUBLOOP; j++) {
                        lfsr = (lfsr >> 1) ^ (-(lfsr & 1u) & 0xd0000001u);
                        u32 index = lfsr & mask;
#define READ(INC) acc ^= data[index].word[INC];
                        PATTERN
#undef READ

                }
                ticks end = getticks();
                times[i] = (elapsed(end, start)-24)/SUBLOOP;
        }

        return acc;
}

int main (int argc, char ** argv)
{
        u32 n = 1000000;
        u32 size = (64*1024);

        if (argc > 1)
                n = atoi(argv[1]);
        if (argc > 2)
                size = atoi(argv[2])*1024UL;
        if (argc > 3)
                path = argv[3];

        double * times = alloc(sizeof(times)*n);
        struct cache_line * data = alloc(size);
        assert(!(size & (size-1)));
        bench(n, (size/sizeof(struct cache_line))-1, times, data);

        print_histogram(times, n);

        return 0;
}
