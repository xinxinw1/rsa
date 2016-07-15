(defpackage :rsa
  (:use :common-lisp :lisp-unit)
  (:export :real2bin
           :gcdcert
           :solve-lin-con
           :mod-pow
           :fermat-prime?
           :miller-rabin-prime?
           :prime?
           :random-prime
           :random-coprime
           :make-rsa-keys
           :gen-pqe
           :gen-rsa-keys
           :encrypt
           :decrypt))

(in-package :rsa)

(setq *print-failures* t)
(setq *print-summary* t)

; returns list of 1 and 0, least significant bit at head
(defun real2bin (a)
  (if (eql a 0) nil
      (let ((r (mod a 2)))
        (cons r (real2bin (/ (- a r) 2))))))

(defun gcdcert-helper (x0 y0 r0 x1 y1 r1)
  (if (eql r1 0) (list x0 y0 r0)
      (let* ((rn (mod r0 r1)) (quot (/ (- r0 rn) r1)))
        (gcdcert-helper x1 y1 r1 (- x0 (* quot x1)) (- y0 (* quot y1)) rn))))

(defun gcdcert (a b)
  (gcdcert-helper 1 0 a 0 1 b))

; returns whether a divides b
(defun divides (a b)
  (eql (mod b a) 0))

(defun coprime (a b)
  (eql (gcd a b) 1))

; solve ax = c (mod m)
; returns (c' m') where x = c' (mod m')
(defun solve-lin-con (a c m)
  (destructuring-bind (x nil g) (gcdcert a m)
    (if (divides g c) (mod (* (/ c g) x) (/ m g))
        -1)))

(defun mod-pow-helper (next bin result m)
  (if (null bin) result
      (mod-pow-helper
        (mod (* next next) m)
        (cdr bin)
        (if (eql (car bin) 1) (mod (* result next) m)
                              result)
        m)))

(defun mod-pow (a b m)
  (mod-pow-helper a (real2bin b) 1 m))

; n, a are integers, (gcd n a) = 1
; https://en.wikipedia.org/wiki/Fermat_pseudoprime
(defun fermat-prime? (n a)
  (eql (mod-pow a (- n 1) n) 1))

(defun list2hash-helper (a tab)
  (if (null a) tab
      (let ((k (car a)) (v (cadr a)))
        (setf (gethash k tab) v)
        (list2hash-helper (cddr a) tab))))

(defun list2hash (a)
  (list2hash-helper a (make-hash-table)))

(defun miller-rabin-prime?-helper (d n a)
  (cond ((eql (mod-pow a d n) (- n 1)) t)
        ((evenp d)
           (miller-rabin-prime?-helper (/ d 2) n a))
        (t (eql (mod-pow a d n) 1))))

(defun miller-rabin-prime? (n a)
  (cond ((eql n 2) t)
        ((evenp n) nil)
        (t (miller-rabin-prime?-helper (/ (- n 1) 2) n a))))

(defun prime? (a)
  (fermat-prime? a 2))

; generates a random number in [min, max)
(defun random-range (min max)
  (+ min (random (- max min))))

(defun random-prime (min max)
  (let ((a (random-range min max)))
    (if (prime? a) a
        (random-prime min max))))

(defun random-coprime (b min max)
  (let ((a (random-range min max)))
    (if (coprime a b) a
        (random-coprime b min max))))

(defun make-rsa-nums (p q e)
  (let ((d (solve-lin-con e 1 (* (- p 1) (- q 1))))
        (n (* p q)))
    (list e d n)))

(defun make-rsa-keys (p q e)
  (destructuring-bind (e d n) (make-rsa-nums p q e)
    (list (list e n) (list d n))))

(defun gen-pqe (prime-size e-size)
  (let ((p (random-prime (expt 10 prime-size) (expt 10 (+ prime-size 1))))
        (q (random-prime (expt 10 prime-size) (expt 10 (+ prime-size 1)))))
    (let* ((phi (* (- p 1) (- q 1)))
           (e (random-coprime phi (expt 10 e-size) (expt 10 (+ e-size 1)))))
      (list p q e))))

(defun gen-rsa-keys (prime-size e-size)
  (destructuring-bind (p q e) (gen-pqe prime-size e-size)
    (make-rsa-keys p q e)))

(defun encrypt (pub m)
  (mod-pow m (car pub) (cadr pub)))

(defun decrypt (pri c)
  (mod-pow c (car pri) (cadr pri)))

