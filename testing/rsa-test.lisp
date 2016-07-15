(defpackage :rsa-test
  (:use :common-lisp :lisp-unit :rsa))

(in-package :rsa-test)

(defun list2hash-helper (a tab)
  (if (null a) tab
      (let ((k (car a)) (v (cadr a)))
        (setf (gethash k tab) v)
        (list2hash-helper (cddr a) tab))))

(defun list2hash (a)
  (list2hash-helper a (make-hash-table)))

(setq *print-failures* t)
(setq *print-summary* t)

(define-test real2bin
  (assert-equal '() (real2bin 0))
  (assert-equal '(1) (real2bin 1))
  (assert-equal '(0 1) (real2bin 2))
  (assert-equal '(1 1) (real2bin 3))
  (assert-equal '(0 0 1) (real2bin 4))
  (assert-equal '(1 0 1) (real2bin 5))
  (assert-equal '(0 1 1) (real2bin 6))
  (assert-equal '(1 1 1) (real2bin 7))
  (assert-equal '(0 0 0 1) (real2bin 8)))

(define-test gcdcert
  (assert-equal '(0 1 1) (gcdcert 1 1))
  (assert-equal '(1 0 1) (gcdcert 1 0))
  (assert-equal '(0 1 1) (gcdcert 0 1))
  (assert-equal '(1 -2 6) (gcdcert 54 24))
  (assert-equal '(1 -2 1) (gcdcert 7 3))
  (assert-equal '(1 -1 5) (gcdcert 20 15))
  (assert-equal '(3 -17 4) (gcdcert 432 76))
  (assert-equal '(5 -14 24) (gcdcert 744 264)))

(define-test solve-lin-con
  (assert-equal 6 (solve-lin-con 5 2 7))
  (assert-equal 2 (solve-lin-con 1 2 7))
  (assert-equal -1 (solve-lin-con 3 2 6))
  (assert-equal 3 (solve-lin-con 3 2 7))
  (assert-equal 3 (solve-lin-con 3 9 7))
  
  ; class examples
  (assert-equal 4 (solve-lin-con 2 3 5))
  (assert-equal 2 (solve-lin-con 884 130 273))
  (assert-equal 75 (solve-lin-con 5 39 168))
  (assert-equal 10 (solve-lin-con 20 11 27))
  (assert-equal 69 (solve-lin-con 2016 3360 4242))
  (assert-equal -1 (solve-lin-con 2016 2424 4242))
  (assert-equal 39 (solve-lin-con 79 22 161)))

(define-test mod-pow
  (assert-equal 5 (mod-pow 7 723 13))
  (assert-equal 4 (mod-pow 56 1 13))
  (assert-equal 452 (mod-pow 23524 152 1234))
  (assert-equal 100174934273296 (mod-pow 23524235242 11273851274932754 123428357274328)))

(defparameter *primes-under-100*
  '(2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97))

(defparameter *fermat-pseudoprimes*
  (list2hash '(
    3 (91)
    4 (15 85 91)
    5 (4)
    6 (35)
    7 (6 25)
    8 (9 21 45 63 65)
    9 (4 8 28 52 91)
    10 (9 33 91 99))))

(define-test fermat-prime?
  (loop for a from 2 to 10 do
    (loop for n from 2 to 100 do
      (if (coprime a n)
        (if (or (find n *primes-under-100*)
                (find n (gethash a *fermat-pseudoprimes*)))
            (assert-true (fermat-prime? n a))
            (assert-false (fermat-prime? n a)))))))

(defparameter *miller-rabin-pseudoprimes*
  (list2hash '(
    7 (25)
    8 (9 65)
    9 (91)
    10 (9 91))))

(define-test miller-rabin-prime?
  (loop for a from 2 to 10 do
    (loop for n from 2 to 100 do
      (if (coprime a n)
        (if (or (find n *primes-under-100*)
                (find n (gethash a *miller-rabin-pseudoprimes*)))
            (assert-true (miller-rabin-prime? n a))
            (assert-false (miller-rabin-prime? n a)))))))

(define-test random-size
  (assert-equal 300 (length (write-to-string (random-size 300)))))

(define-test make-rsa-nums
  (assert-equal '(19 1099 4307) (make-rsa-nums 59 73 19))
  (assert-equal '(4567 103 9797) (make-rsa-nums 97 101 4567)))

(define-test gen-rsa-keys
  (assert-equal 1234567
    (let ((key (gen-rsa-keys 30 5)))
      (decrypt (cadr key) (encrypt (car key) 1234567))))
  (assert-equal 1234567
    (let ((key (gen-rsa-keys 30 5)))
      (encrypt (cadr key) (decrypt (car key) 1234567)))))