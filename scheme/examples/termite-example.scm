;; -module(ring).
;; -export([make_ring/2]).

;; make_ring(N, M) -> spawn(fun() -> sender(N, M) end).

;; sender(N, M) ->
;;     FirstPid = self(),
;;     NextPid = spawn(fun() -> tunnel(N, 2, FirstPid) end),
;;     statistics(runtime),
;;     statistics(wall_clock),
;;     do_times(M, 0, fun(I) -> NextPid ! I end),
;;     NextPid ! done,
;;     receive done -> done end,
;;     {_, Time1} = statistics(runtime),
;;     {_, Time2} = statistics(wall_clock),
;;     U1 = Time1 * 1000,
;;     U2 = Time2 * 1000,
;;     io:format("Total time=~p (~p) microseconds~n", [U1, U2]).

;; do_times(N, N, _) -> done;
;; do_times(N, J, Fun) -> Fun(J), do_times(N, J+1, Fun).

;; tunnel(N, N, FirstPid) -> tunnel(FirstPid);
;; tunnel(N, J, FirstPid) ->
;;      tunnel(spawn(fun() -> tunnel(N, J+1, FirstPid) end)).

;; tunnel(Pid) ->
;;     receive
;;         done -> Pid ! done;
;;         Any -> Pid ! Any, tunnel(Pid)
;;     end.

(define (make-ring n m) (spawn (lambda () (sender n m))))

(define (sender n m)
  (define (display-time msg start end)
    (display (list msg (- end start)))
    (newline))
  (let* ((fist-pid (self))
         (next-pid (spawn (lambda () (tunnel n 2 fist-pid))))
         (rt (real-time))
         (ct (cpu-time)))
    (let loop ((m m))
      (cond ((= 0 m)
             (! next-pid 'done)
             (recv ('done
                    (let ((nct (cpu-time))
                          (nrt (real-time)))
                      (display-time "CPU time: " ct nct)
                      (display-time "Real time: " rt nrt)))))
            (else (! next-pid m)
                  (loop (- m 1)))))))

(define (tunnel n j first-pid)
  (cond ((= n j) (send-recv first-pid))
        (else (send-recv (spawn (lambda ()
                                   (tunnel n (+ 1 j) first-pid)))))))

(define (send-recv pid)
  (recv
   ('done (! pid 'done))
   (x (! pid x) (send-recv pid))))
