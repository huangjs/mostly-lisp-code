(asdf:defsystem #:stm
  :components ((:file "package")
               (:file "vops" :depends-on ("package"))
               (:file "tls" :depends-on ("package"))
               (:file "actions" :depends-on ("package"))
               (:file "seqlock" :depends-on ("package"
                                             "vops"
                                             "tls"
                                             "actions"))
               (:file "stm"  :depends-on ("package"
                                          "vops"
                                          "tls"
                                          "actions"
                                          "seqlock"))
               (:file "spinlock" :depends-on ("package"
                                              "stm"))
               (:file "bytelock" :depends-on ("package"
                                              "vops"
                                              "stm"))
               (:file "cell" :depends-on ("package"
                                          "stm"
                                          #+nil "spinlock"
                                          "bytelock"))))
