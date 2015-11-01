#include "semaphore.h"
#define EVENTS_INTERNALS
typedef semaphore_t * event_t;

#include "events.h"

int event_init(event_t * event)
{
        return sb_sem_init(event, 0);
}

int event_destroy(event_t event)
{
        return sb_sem_destroy(event);
}

int event_signal(event_t event)
{
        return sb_sem_post(event);
}

int event_wait(event_t event, unsigned long sec,  unsigned long ns)
{
        return sb_sem_timedwait(event, sec, ns);
}

int event_broadcast(event_t event)
{
#ifdef HAVE_SB_SEM_BROADCAST
        return sb_sem_broadcast(event);
#else
        return event_signal(event);
#endif
}

int event_resignal(event_t event)
{
        return event_signal(event);
}

int event_clear(event_t event)
{
#ifdef HAVE_SB_SEM_BROADCAST
        (void)event;
        return 0;
#elif defined(HAVE_SB_SEM_TRYWAIT) && defined(HAVE_SB_SEM_VALUE)
        int count = sb_sem_value(event);
        int i, err;
        for (i = 0; i < count; i++)
                if ((err = sb_sem_trywait(event)))
                        return err;
        return 0;
#else
#       error "Unable to construct event_clear"
#endif
}
