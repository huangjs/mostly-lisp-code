#ifndef EVENTS_H
#define EVENTS_H

#ifndef EVENTS_INTERNALS
struct event;
typedef struct event * event_t;
#endif

int event_init (event_t*);
int event_destroy (event_t);

/* signal to signal one waiter
 * broadcast to signal all threads
 * resignal to propagate broadcast
 */
int event_signal(event_t);
int event_broadcast(event_t);
int event_resignal(event_t);

/* Wait on event for up to timeout nanoseconds.
 * Might return spuriously.
 */
int event_wait(event_t, unsigned long timeout_sec, unsigned long timeout_nsec);

/* Clear the event of any signalled/broadcast
 * value.
 *
 * Only really needed for implementations based
 * on things like FDs.
 */
int event_clear(event_t);
#endif
