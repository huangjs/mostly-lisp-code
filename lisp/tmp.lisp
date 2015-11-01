(labels-tc ((oddp (n)
           (declare (fixnum n))
           (if (= n 0)
               nil
               (evenp (1- n))))
         (evenp (m)
           (declare (fixnum m))
           (if (= m 0)
               t
               (oddp (1- m)))))
  (print (oddp 21)))

=>

(labels ((#:entry (#:name &key n m)
           (block #:exit
             (tagbody
                #:dispatch
                (case #:name
                  (oddp (go #:oddp))
                  (evenp (go #:evenp)))
                #:oddp
                (return-from #:exit
                  (locally
                      (declare (fixnum n))
                    (if (= n 0)
                        nil
                        (progn
                          (psetf m (1- n))
                          (go #:evenp)))))
                #:evenp
                (return-from #:exit
                  (locally
                      (declare (fixnum m))
                    (if (= m 0)
                        t
                        (progn
                          (psetf n (1- m))
                          (go #:oddp))))))))
         (oddp (n)
           (#:entry 'oddp :n n))
         (evenp (m)
           (#:entry 'evenp :m m)))
  (print (oddp 21)))

