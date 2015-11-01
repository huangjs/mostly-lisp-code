#include <mach/mach_init.h>
#include <mach/task.h>
#include <mach/semaphore.h>
#define SEMAPHORE_INTERNALS
#include "semaphore.h"

/* Bad pun alert!
 * 
 * The interface pretends semaphores are internally represented as
 * word-aligned pointers.  Semaphores in Mach (ports, actually), are
 * just ints.  We make sure they're small enough for fixnums
 *
 * On x86-64, it's fine, because semaphores are 32 bit integers;
 * x86-32, not necessarily.
 */

int sb_sem_init(semaphore_t ** semaphore, int value)
{
        semaphore_t sem;
        int ret = semaphore_create(mach_task_self(), &sem, SYNC_POLICY_FIFO, value);
        if ((unsigned long)sem >= 1UL<<(8*sizeof(unsigned long)-4)) {
                semaphore_destroy(mach_task_self(), sem);
                return -1;
        }
        *semaphore = (semaphore_t*)(unsigned long)sem;
        return ret;
}

#define GET_SEM(X) ((semaphore_t)((unsigned long)X))

int sb_sem_destroy(semaphore_t * semaphore)
{
        return semaphore_destroy(mach_task_self(), GET_SEM(semaphore));
}

int sb_sem_post(semaphore_t * semaphore)
{
        return semaphore_signal(GET_SEM(semaphore));
}

int sb_sem_broadcast(semaphore_t * semaphore)
{
        return semaphore_signal_all(GET_SEM(semaphore));
}

int sb_sem_wait(semaphore_t * semaphore)
{
        return semaphore_wait(GET_SEM(semaphore));
}

int sb_sem_timedwait(semaphore_t * semaphore, unsigned long sec, unsigned long ns)
{
        return semaphore_timedwait(GET_SEM(semaphore), 
                                   (mach_timespec_t){sec, ns});
}
