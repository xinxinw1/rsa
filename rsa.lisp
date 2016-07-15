(defpackage :rsa
  (:use :common-lisp)
  (:export :coprime
           :gcdcert
           :solve-lin-con
           :real2bin
           :mod-pow
           :test-small-primes
           :fermat-prime?
           :miller-rabin-prime?
           :prime?
           :random-range
           :random-size
           :random-prime
           :random-coprime
           :make-rsa-nums
           :make-rsa-keys
           :gen-pqe
           :gen-rsa-keys
           :encrypt
           :decrypt))

(in-package :rsa)

;; Helper Functions

(defun coprime (a b)
  (eql (gcd a b) 1))

(defun gcdcert-helper (x0 y0 r0 x1 y1 r1)
  (if (eql r1 0) (list x0 y0 r0)
      (let* ((rn (mod r0 r1)) (quot (/ (- r0 rn) r1)))
        (gcdcert-helper x1 y1 r1 (- x0 (* quot x1)) (- y0 (* quot y1)) rn))))

(defun gcdcert (a b)
  (gcdcert-helper 1 0 a 0 1 b))

; returns whether a divides b
(defun divides (a b)
  (eql (mod b a) 0))

; solve ax = c (mod m)
; returns (c' m') where x = c' (mod m')
(defun solve-lin-con (a c m)
  (destructuring-bind (x nil g) (gcdcert a m)
    (if (divides g c) (mod (* (/ c g) x) (/ m g))
        -1)))

; returns list of 1 and 0, least significant bit at head
(defun real2bin (a)
  (if (eql a 0) nil
      (let ((r (mod a 2)))
        (cons r (real2bin (/ (- a r) 2))))))

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

;; Primality tests

(defparameter *small-primes*
  '(2 3 5 7 11 13 17 19 23 29
    31 37 41 43 47 53 59 61 67 71
    73 79 83 89 97 101 103 107 109 113
    127 131 137 139 149 151 157 163 167 173
    179 181 191 193 197 199 211 223 227 229
    233 239 241 251 257 263 269 271 277 281
    283 293 307 311 313 317 331 337 347 349
    353 359 367 373 379 383 389 397 401 409
    419 421 431 433 439 443 449 457 461 463
    467 479 487 491 499 503 509 521 523 541
    547 557 563 569 571 577 587 593 599 601
    607 613 617 619 631 641 643 647 653 659
    661 673 677 683 691 701 709 719 727 733
    739 743 751 757 761 769 773 787 797 809
    811 821 823 827 829 839 853 857 859 863
    877 881 883 887 907 911 919 929 937 941
    947 953 967 971 977 983 991 997))

(defun test-small-primes-helper (a primes)
  (if (null primes) t
      (if (divides (car primes) a) nil
          (test-small-primes-helper a (cdr primes)))))

(defun test-small-primes (a)
  (test-small-primes-helper a *small-primes*))

; n, a are integers, (gcd n a) = 1
; https://en.wikipedia.org/wiki/Fermat_pseudoprime
(defun fermat-prime? (n a)
  (eql (mod-pow a (- n 1) n) 1))

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
  (let ((t1 (test-small-primes a)))
    (values (and t1 (miller-rabin-prime? a 2)) t1)))

;; Random number functions

; generates a random number in [min, max)
(defun random-range (min max)
  (+ min (random (- max min))))

(defun random-size (s)
  (random-range (expt 10 (- s 1)) (expt 10 s)))

#|
(defun random-prime (s)
  (let ((a (random-size s)))
    (if (prime? a) a
        (random-prime s))))
|#

(defun random-prime-helper (s n)
  (let ((a (random-size s)))
    (multiple-value-bind (prime used-2nd-test) (prime? a)
      (let ((new-n (if used-2nd-test (+ n 1) n)))
        (if prime (values a new-n)
            (random-prime-helper s new-n))))))

(defun random-prime (s)
  (random-prime-helper s 0))

(defun random-coprime (b s)
  (let ((a (random-size s)))
    (if (coprime a b) a
        (random-coprime b s))))

;; RSA Key Generation

(defun make-rsa-nums (p q e)
  (let ((d (solve-lin-con e 1 (* (- p 1) (- q 1))))
        (n (* p q)))
    (list e d n)))

(defun make-rsa-keys (p q e)
  (destructuring-bind (e d n) (make-rsa-nums p q e)
    (list (list e n) (list d n))))

(defun gen-pqe (prime-size e-size)
  (multiple-value-bind (p pn) (random-prime prime-size)
    (multiple-value-bind (q qn) (random-prime prime-size)
      (let* ((phi (* (- p 1) (- q 1)))
             (e (random-coprime phi e-size)))
        (values (list p q e) pn qn)))))

(defun gen-rsa-keys (prime-size e-size)
  (destructuring-bind (p q e) (gen-pqe prime-size e-size)
    (make-rsa-keys p q e)))

;; Encryption and Decryption

(defun encrypt (pub m)
  (mod-pow m (car pub) (cadr pub)))

(defun decrypt (pri c)
  (mod-pow c (car pri) (cadr pri)))

