(defpackage :rsa
  (:use :common-lisp)
  (:export :coprime
           :gcdcert
           :solve-lin-con
           :real2bin
           :mod-pow
           :divide-twos
           :lucas-u
           :fast-lucas
           :mod-lucas
           :jacobi
           :prime-sieve
           :perfect-square?
           :find-lucas-d
           :find-lucas-pq
           :test-small-primes
           :fermat-prime?
           :miller-rabin-prime?
           :lucas-prime-pq?
           :lucas-prime?
           :prime?
           :random-range
           :random-size
           :random-prime
           :random-coprime
           :make-rsa-nums
           :make-rsa-keys
           :gen-pqe
           :gen-rsa-nums
           :gen-rsa-keys
           :encrypt-num
           :decrypt-num
           :char-to-3-digit-ascii
           :encode-to-str
           :encode
           :group-by-3
           :decode-from-str
           :decode
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
  
(defun divide-twos-helper (d s)
  (cond ((eql d 0) (list 0 0))
        ((oddp d) (list s d))
        (t (divide-twos-helper (/ d 2) (+ s 1)))))

; returns (s d) such that n = 2^s*d
(defun divide-twos (n)
  (divide-twos-helper n 0))

; returns U(n)
(defun lucas-u (p q)
  (lambda (n)
    (labels ((lucas-u-helper (currn u0 u1)
               (if (eql currn n) u1
                   (lucas-u-helper (+ currn 1) u1 (- (* p u1) (* q u0))))))
      (if (eql n 0) 0
          (lucas-u-helper 1 0 1)))))

(defun fast-lucas-helper (bin u v qpow p q d)
  (if (null bin) (list u v)
      (fast-lucas-helper2
        bin
        (* u v)
        (- (* v v) (* 2 qpow))
        (* qpow qpow)
        p q d)))
  
(defun fast-lucas-helper2 (bin u v qpow p q d)
  (if (eql (car bin) 1)
      (fast-lucas-helper
        (cdr bin)
        (/ (+ (* p u) v) 2)
        (/ (+ (* d u) (* p v)) 2)
        (* qpow q)
        p q d)
      (fast-lucas-helper
        (cdr bin)
        u
        v
        qpow
        p q d)))

; returns (U(n) V(n))
(defun fast-lucas (n p q)
  (fast-lucas-helper (nreverse (real2bin n)) 0 2 1 p q (- (* p p) (* 4 q))))


(defun mod-lucas-double (u v qpow m)
  (list (mod (* u v) m) (mod (- (* v v) (* 2 qpow)) m) (mod (* qpow qpow) m)))

; assumes m is odd
(defun mod-lucas-plus1 (u v qpow p q d m)
  (let* ((u2 (+ (* p u) v))
         (adju2 (if (oddp u2) (+ u2 m) u2))
         (v2 (+ (* d u) (* p v)))
         (adjv2 (if (oddp v2) (+ v2 m) v2)))
    (list (mod (/ adju2 2) m)
          (mod (/ adjv2 2) m)
          (mod (* qpow q) m))))

(defun mod-lucas-helper (bin u v qpow p q d m)
  ;(format t "u: ~S v: ~S qpow: ~S~%" u v qpow)
  (if (null bin) (values (list u v) qpow)
      (destructuring-bind (newu newv newqpow) (mod-lucas-double u v qpow m)
        (mod-lucas-helper2 bin newu newv newqpow p q d m))))
  
(defun mod-lucas-helper2 (bin u v qpow p q d m)
  ;(format t "u: ~S v: ~S qpow: ~S~%" u v qpow)
  (if (eql (car bin) 1)
      (destructuring-bind (newu newv newqpow) (mod-lucas-plus1 u v qpow p q d m)
        (mod-lucas-helper (cdr bin) newu newv newqpow p q d m))
      (mod-lucas-helper (cdr bin) u v qpow p q d m)))

; returns (U(n) V(n)) (mod m)
; we assume gcd(m, 2) = 1 (ie. m must be odd)
(defun mod-lucas (n p q m)
  (if (evenp m) (error "MOD-LUCAS: ~S must be an odd number." m)
      (mod-lucas-helper
        (nreverse (real2bin n))
        0 2 1
        (mod p m)
        (mod q m)
        (mod (- (* p p) (* 4 q)) m)
        m)))

; assume n is odd
(defun jacobi-step1 (a n)
  (if (eql n 1) 1
      (jacobi-step2 (mod a n) n)))

; (jacobi 2 n), n is odd, n != 1
(defun jacobi-top2 (n)
  (let ((m (mod n 8)))
    (if (or (eql m 1) (eql m 7)) 1
        -1)))

; at this point, a < n
(defun jacobi-step2 (a n)
  (cond ((eql a 0) 0)
        ((evenp a) (* (jacobi-top2 n) (jacobi-step2 (/ a 2) n)))
        ((jacobi-step3 a n))))

; at this point a < n, a odd
(defun jacobi-step3 (a n)
  (cond ((eql a 1) 1)
        ((not (coprime a n)) 0)
        (t (jacobi-step4 a n))))

; (* (jacobi n m) (jacobi m n))
(defun jacobi-prod-reci (n m)
  (if (or (eql (mod n 4) 1) (eql (mod m 4) 1))
      1
      -1))

; at this point, a < n, a and n are both odd and coprime
(defun jacobi-step4 (a n)
  (* (jacobi-prod-reci a n) (jacobi-step1 n a)))

(defun jacobi (a n)
  (if (evenp n) (error "JACOBI: ~S must be an odd number." n)
      (jacobi-step1 a n)))

(defun get-first-unmarked-after (p arr)
  (let ((start (if p p 2)))
    (loop for i from start below (length arr) do
      (if (elt arr i) (return-from get-first-unmarked-after i)))))

(defun mark-all-multiples (p arr)
  (loop for i from p below (length arr) by p do
    (setf (elt arr i) nil)))

; indexes 2 to n -> 0 to n-2, len n-1
(defun prime-sieve (n)
  (let ((primes nil) (arr (make-array n :initial-element t)))
    (loop
      (let ((p (get-first-unmarked-after (car primes) arr)))
        (if (not p) (return-from prime-sieve (nreverse primes))
            (progn (push p primes)
                   (mark-all-multiples p arr)))))))

(defun perfect-square? (n)
  (eql (expt (isqrt n) 2) n))

; find d such that (jacobi d n) = -1
; n must not be even
(defun find-lucas-d (n)
  (if (perfect-square? n) nil
      (loop for i from 1
            for absd from 5 by 2
            for d = (if (oddp i) absd (- absd)) do
        ;(if (> i 100) (progn (print "what") (return-from find-lucas-d nil)))
        (let ((j (jacobi d n)))
          (cond ((and (eql j 0) (not (eql n absd))) (return-from find-lucas-d nil))
                ((eql (jacobi d n) -1) (return-from find-lucas-d d)))))))

; n must not be even
(defun find-lucas-pq (n)
  (let ((d (find-lucas-d n)))
    (if (not d) nil
        (list 1 (/ (- 1 d) 4) d))))
    
;; Primality tests

(defparameter *small-primes*
  (prime-sieve 1000))

(defun test-small-primes-helper (a primes)
  (if (or (null primes) (>= (car primes) a)) t
      (if (divides (car primes) a) nil
          (test-small-primes-helper a (cdr primes)))))

(defun test-small-primes (a)
  (test-small-primes-helper a *small-primes*))

; n, a are integers, (gcd n a) = 1
; https://en.wikipedia.org/wiki/Fermat_pseudoprime
(defun fermat-prime? (n a)
  (eql (mod-pow a (- n 1) n) 1))

; curr = a^(2^r*d), curr has already been checked
(defun miller-rabin-prime?-helper (curr n r s)
  (if (eql r (- s 1)) nil
      (let ((newcurr (mod (* curr curr) n)))
        (if (eql newcurr (- n 1)) t
            (miller-rabin-prime?-helper newcurr n (+ r 1) s)))))

(defun miller-rabin-prime? (n a)
  (cond ((eql n 2) t)
        ((evenp n) nil)
        (t (destructuring-bind (s d) (divide-twos (- n 1))
             (let ((curr (mod-pow a d n)))
               (cond ((eql curr 1) t)
                     ((eql curr (- n 1)) t)
                     (t (miller-rabin-prime?-helper curr n 0 s))))))))

#|(defun miller-rabin-prime?-helper (d n a)
  (cond ((eql (mod-pow a d n) (- n 1)) t)
        ((evenp d)
           (miller-rabin-prime?-helper (/ d 2) n a))
        (t (eql (mod-pow a d n) 1))))

(defun miller-rabin-prime? (n a)
  (cond ((eql n 2) t)
        ((evenp n) nil)
        (t (miller-rabin-prime?-helper (/ (- n 1) 2) n a))))|#
        
(defun lucas-prime-pqdj? (n p q d j)
  (if (and (eql j 0) (not (eql d n))) nil
      (eql (car (mod-lucas (- n j) p q n)) 0)))

; n, a integers, (gcd n q) = 1
(defun lucas-prime-pq? (n p q)
  (let* ((d (- (* p p) (* 4 q)))
        (j (jacobi d n)))
    (lucas-prime-pqdj? n p q d j)))

(defun lucas-prime? (n)
  (cond ((eql n 2) t)
        ((evenp n) nil)
        (t (let ((pqd (find-lucas-pq n)))
             (if (not pqd) nil
                 (destructuring-bind (p q d) pqd
                   (if (not (coprime n q)) nil
                       (lucas-prime-pqdj? n p q d -1))))))))

(defun prime? (a)
  (let ((t1 (test-small-primes a)))
    (if (not t1) (values t1 t1 nil)
        (let ((t2 (miller-rabin-prime? a 2)))
          (if (not t2) (values t2 t1 t2)
              (values (lucas-prime? a) t1 t2))))))

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

(defun random-prime-helper (s n m)
  (let ((a (random-size s)))
    (multiple-value-bind (prime used-2nd-test used-3rd-test) (prime? a)
      (let ((new-n (if used-2nd-test (+ n 1) n))
            (new-m (if used-3rd-test (+ m 1) m)))
        (if prime (values a new-n new-m)
            (random-prime-helper s new-n new-m))))))

(defun random-prime (s)
  (random-prime-helper s 0 0))

(defun random-coprime (b s)
  (let ((a (random-size s)))
    (if (coprime a b) a
        (random-coprime b s))))

;; RSA Key Generation

(defun make-rsa-nums (p q e)
  (let ((d (solve-lin-con e 1 (* (- p 1) (- q 1))))
        (n (* p q)))
    (list e d n)))

(defun rsa-nums-to-keys (l)
  (destructuring-bind (e d n) l
    (list (list e n) (list d n))))

(defun make-rsa-keys (p q e)
  (rsa-nums-to-keys (make-rsa-nums p q e)))

(defun gen-pqe (prime-size e-size)
  ; according to Wikipedia, prime size should differ by a few digits
  (multiple-value-bind (p pn) (random-prime (+ prime-size 3))
    (multiple-value-bind (q qn) (random-prime prime-size)
      (let* ((phi (* (- p 1) (- q 1)))
             (e (random-coprime phi e-size)))
        (values (list p q e) pn qn)))))

(defun gen-rsa-nums (prime-size e-size)
  (apply #'make-rsa-nums (gen-pqe prime-size e-size)))

(defun gen-rsa-keys (prime-size e-size)
  (rsa-nums-to-keys (gen-rsa-nums prime-size e-size)))

;; Encryption and Decryption

(defun encrypt-num (pub m)
  (destructuring-bind (e n) pub
    (if (>= m n)
          (error "ENCRYPT-NUM: Message must be less than n")
        (mod-pow m e n))))

(defun decrypt-num (pri c)
  (mod-pow c (car pri) (cadr pri)))

;; Text to number

(defun char-to-3-digit-ascii (c)
  (format nil "~3,'0d" (char-code c)))

; encode string to string
(defun encode-to-str (str)
  (apply #'concatenate (cons 'string (loop for c across str collect (char-to-3-digit-ascii c)))))

(defun str-to-num (str)
  (let ((n (parse-integer str :junk-allowed t)))
    (if (not n) 0 n)))

; encode string to integer
(defun encode (str)
  (str-to-num (encode-to-str str)))

(defun subseq-check-end (str start end)
  (cond ((> end (length str))
           (subseq-check-end str start (length str)))
        ((< start 0) (subseq-check-end str 0 end))
        (t (subseq str start end))))

(defun group-by-3 (str)
  (nreverse (loop for i downfrom (length str) to 1 by 3
    collect (subseq-check-end str (- i 3) i))))

(defun decode-from-str (str)
  (map 'string #'code-char (map 'list #'str-to-num (group-by-3 str))))

(defun num-to-str (num)
  (if (eql num 0) ""
      (write-to-string num)))

(defun decode (num)
  (decode-from-str (num-to-str num)))

; m is a string
; returns num
(defun encrypt (pub m)
  (encrypt-num pub (encode m)))

; c is a num
; returns string
(defun decrypt (pri c)
  (decode (decrypt-num pri c)))