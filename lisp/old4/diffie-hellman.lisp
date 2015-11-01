;; So to use it, you and your partner each compute a secret key.  Then
;; you each use the secret key to compute a public key, which you give
;; to the other person.  This public key doesn't have to be kept
;; secret.  Then you use the other person's public key and your secret
;; key to compute a "common key".  The other person will use your
;; public key and his secret key to make the same computation.  You
;; will both wind up with an identical "common key".  Each of you
;; could then use a symmetrical encryption algorithm, with the common
;; key as the encryption key, to exchange messages.

(defconstant *dh-base* 3)

(defconstant *dh-modulus* #xde9b707d4c5a4633c0290c95ff30a605aeb7ae864ff48370f13cf01d49adb9f23d19a439f753ee7703cf342d87f431105c843c78ca4df639931f3458fae8a94d1687e99a76ed99d0ba87189f42fd31ad8262c54a8cf5914ae6c28c540d714a5f6087a171fb74f4814c6f968d72386ef356a05180c3bec7ddd5ef6fe76b0531c3)

;;; Stolen from clmath package by Gerald Roylance.
(defun modpower (number exponent modulus)
  (declare (integer number exponent modulus))
  (do ((exp  exponent  (floor exp 2))
       (sqr  number (mod (* sqr sqr) modulus))
       (ans  1))
      ((zerop exp) ans)
    (declare (integer exp sqr ans))
    (if (oddp exp)
        (setq ans (mod (* ans sqr) modulus)))))

(defun compute-secret-key (dh-modulus)
  (declare (integer dh-modulus))
  (random dh-modulus (make-random-state t)))

(defun compute-public-key (base secret-key modulus)
  (declare (integer base secret-key modulus))
  (modpower base secret-key modulus))

(defun compute-common-key (remote-public-key local-secret-key modulus)
  (declare (integer remote-public-key local-secret-key modulus))
  (modpower remote-public-key local-secret-key modulus)) 
