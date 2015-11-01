#define RUNME /*
for hash in -UHASH -DHASH; do
  for base in -UBASE -DBASE; do
    for direct in -UDIRECT -DDIRECT; do
      gcc -Wall -std=gnu99 -g -O3 $hash $base $direct $CFLAGS $* branch.c -o branch$hash$base$direct || exit $?
    done
  done
done
exit 0

for hash in -UHASH -DHASH; do
  for base in -UBASE -DBASE; do
    for direct in -UDIRECT -DDIRECT; do
      echo branch$hash$base$direct
      for i in 0 1 2 3 4 5 6 7 8 9; do 
        time ./branch$hash$base$direct
      done
    done
  done
done
*/

#define __STRINGIFY(x) #x
#define __TOSTRING(x)  __STRINGIFY(x)
#define __FILE_LINE__  __FILE__ ":" __TOSTRING(__LINE__)

#include <inttypes.h>
#include <stdio.h>
#include <stdlib.h>

size_t hash (size_t) __attribute__ ((__always_inline__));
size_t shuffle (size_t x) __attribute__ ((__always_inline__));

void target0 () __attribute__ ((noinline));
void target1 () __attribute__ ((noinline));
void target2 () __attribute__ ((noinline));
void target3 () __attribute__ ((noinline));

// Try to defeat slightly smart predictors that'd notice a 
// constant stride in the indirect calls

void target1 ()
{
	size_t i = 0; // minimally do some work
	asm("#" __FILE_LINE__
	    :::"memory");
	i++;
	asm("#" __FILE_LINE__
	    :::"memory");
	i++;
	asm("#" __FILE_LINE__
	    :::"memory");
	i++;
	asm("#" __FILE_LINE__
	    :::"memory");
	i++;
	asm("#" __FILE_LINE__
	    :::"memory");
	i++;
	asm("#" __FILE_LINE__
	    :::"memory");
	i++;
	asm("#" __FILE_LINE__
	    :::"memory");
}

size_t shuffle (size_t x)
{
#ifdef SHUFFLE
	static size_t lfsr = 1;
	lfsr = (lfsr >> 1) 
		^ (-(signed int)(lfsr & 1) & 0xd0000001u);
	lfsr = (lfsr >> 1) 
		^ (-(signed int)(lfsr & 1) & 0xd0000001u);
	return lfsr;
#else
	return x;
#endif
}

void target3 ()
{
	size_t i = 0;
	asm("#" __FILE_LINE__
	    :::"memory");
	i++;
	asm("#" __FILE_LINE__
	    :::"memory");
	i++;
	asm("#" __FILE_LINE__
	    :::"memory");
	i++;
	asm("#" __FILE_LINE__
	    :::"memory");
	i++;
	asm("#" __FILE_LINE__
	    :::"memory");
	i++;
	asm("#" __FILE_LINE__
	    :::"memory");
	i++;
	asm("#" __FILE_LINE__
	    :::"memory");
}


typedef void (*target_t)();

void doiters(size_t) __attribute__ ((noinline));

void
doiters(size_t niter)
{
#ifdef BASE
	static const target_t targets[] 
		__attribute__ ((__aligned__(16))) 
		= {target3, target3, target3, target3};
#else
	static const target_t targets[] 
		__attribute__ ((__aligned__(16))) 
		= {target0, target1, target2, target3};
#endif

	for (size_t i = 0; i < niter; i++) {
		size_t x = shuffle(i)&3;
		target_t target = targets[x];
#ifdef BASE
		x = 2; // must fake hashing on address
		       // don't constant fold it away
		asm volatile ("#" __FILE_LINE__
			      :"=g"(x)   //isn't that eqv to
			      :"0"(x));  // "g"(x) and no "0"(x)?
#endif

// waste some time -- avoid benching difference in
// dependencies
	size_t j = 0;
	asm("#" __FILE_LINE__
	    :"=g"(j):"0"(j));
	j++;
	asm("#" __FILE_LINE__
	    :"=g"(j):"0"(j));
	j++;
	asm("#" __FILE_LINE__
	    :"=g"(j):"0"(j));
	j++;
	asm("#" __FILE_LINE__
	    :"=g"(j):"0"(j));
	j++;
	asm("#" __FILE_LINE__
	    :"=g"(j):"0"(j));
	j++;
	asm("#" __FILE_LINE__
	    :"=g"(j):"0"(j));
	j++;
	asm("#" __FILE_LINE__
	    :"=g"(j):"0"(j));

#ifndef HASH
		asm("#" __FILE_LINE__
		    : "=g"(target)
		    : "0"(target));

# if defined(DIRECT) && defined(BASE)
		target3();
# else
		target();
# endif
#else

# ifdef DIRECT
		if (0 == x) {
			asm("#" __FILE_LINE__ 
			    :"=g"(target)
			    : "0"(target));
			target0();
		} else if (1 == x) {
			asm("#" __FILE_LINE__ 
			    :"=g"(target)
			    : "0"(target));
			target1();
		} else if (2 == x) {
			asm("#" __FILE_LINE__ 
			    :"=g"(target)
			    : "0"(target));
			target2();
		} else {
			asm("#" __FILE_LINE__ 
			    :"=g"(target)
			    : "0"(target));
			target3();
		}
# else
		if (0 == x) {
			asm(".p2align 4\n\t" // 16 byte alignment
			    "call *%0 #" __FILE_LINE__ "\n"
			    :: "r"(target));
		} else if (1 == x) {
			asm(".p2align 4\n\t"
			    "call *%0 #" __FILE_LINE__ "\n"
			    :: "r"(target));
		} else if (2 == x) {
			asm(".p2align 4\n\t"
			    "call *%0 #" __FILE_LINE__ "\n"
			    :: "r"(target));
		} else {
			asm(".p2align 4\n\t"
			    "call *%0 #" __FILE_LINE__ "\n"
			    :: "r"(target));
		}
# endif

#endif
	}
}

#ifndef ITERATIONS
# define ITERATIONS 2000000000
#endif

int main (int argc, char **argv)
{
	size_t niter = argc > 1 ? strtoul(argv[1], NULL, 0) : ITERATIONS;

	doiters(niter);
	return 0;
}

void target2 ()
{
	size_t i = 0;
	asm("#" __FILE_LINE__
	    :::"memory");
	i++;
	asm("#" __FILE_LINE__
	    :::"memory");
	i++;
	asm("#" __FILE_LINE__
	    :::"memory");
	i++;
	asm("#" __FILE_LINE__
	    :::"memory");
	i++;
	asm("#" __FILE_LINE__
	    :::"memory");
	i++;
	asm("#" __FILE_LINE__
	    :::"memory");
	i++;
	asm("#" __FILE_LINE__
	    :::"memory");
}

size_t hash (size_t x)
{
	return ((0xA1C94B23*x) >> 29)&7;
}


void target0 ()
{
	size_t i = 0;
	asm("#" __FILE_LINE__
	    :::"memory");
	i++;
	asm("#" __FILE_LINE__
	    :::"memory");
	i++;
	asm("#" __FILE_LINE__
	    :::"memory");
	i++;
	asm("#" __FILE_LINE__
	    :::"memory");
	i++;
	asm("#" __FILE_LINE__
	    :::"memory");
	i++;
	asm("#" __FILE_LINE__
	    :::"memory");
	i++;
	asm("#" __FILE_LINE__
	    :::"memory");
}

