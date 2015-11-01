;;; The simulation-environment structure
;;; Index  Slot 
;;;   0    running?              #t if the main loop is running
;;;   1    time                  current simulation time
;;;   2    now-event-list        events to be executed now
;;;   3    future-event-list     events to be executed in the future
;;;   4    loop-next             continuation to return to the main loop
;;;   5    loop-exit             continuation to exit the main loop
;;;   6    event                 currently executing event or #f
;;;   7    process               currently executing process or #f
;;; v--- hierarchical environments ----------------------------------v
;;;   8    parent                parent environment
;;;   9    children              list of children environments
;;; v--- continuous simulation --------------------------------------v
;;;   10   continuous-event-list events to be executed continuously
;;;   11   evolve                ode-evolve object
;;;   12   control               ode-control object
;;;   13   step-type             ode-step-type object
;;;   14   step                  ode-step object
;;;   15   system                ode-system-type
;;;   16   step-size             last step-size
;;;   17   dimension             size of the state vector
;;;   18   y                     state vector
;;;   19   dydt                  derivative vector
;;;   20   state-changed?        #t if the state vector needs updating
;;;   21   max-step-size         limit for step-size


