#ifndef SEMAPHORE_H
#define SEMAPHORE_H

#ifndef SEMAPHORE_INTERNALS
typedef struct semaphore_t semaphore_t;
#endif

int sb_sem_init(semaphore_t**, int);
int sb_sem_destroy(semaphore_t*);
int sb_sem_post(semaphore_t*);
int sb_sem_wait(semaphore_t*);
int sb_sem_timedwait(semaphore_t*, unsigned long sec, unsigned long ns);

/* Optional stuff 
 *
 * Mach has broadcast
 * POSIX has trywait and value
 * Windows?
 */

int sb_sem_broadcast(semaphore_t*);
int sb_sem_trywait(semaphore_t*);
int sb_sem_value(semaphore_t*);

#define HAVE_SB_SEM_BROADCAST
#endif
