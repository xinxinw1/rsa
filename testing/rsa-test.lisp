(defpackage :rsa-test
  (:use :common-lisp :lisp-unit :rsa))

(in-package :rsa-test)

(defun list2hash-helper (a tab)
  (if (null a) tab
      (let ((k (car a)) (v (cadr a)))
        (setf (gethash k tab) v)
        (list2hash-helper (cddr a) tab))))

(defun list2hash (a)
  (list2hash-helper a (make-hash-table :test 'equal)))

(setq *print-failures* t)
(setq *print-errors* t)
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

(define-test divide-twos
  (assert-equal '(0 0) (divide-twos 0))
  (assert-equal '(0 1) (divide-twos 1))
  (assert-equal '(1 1) (divide-twos 2))
  (assert-equal '(0 3) (divide-twos 3))
  (assert-equal '(2 1) (divide-twos 4))
  (assert-equal '(0 5) (divide-twos 5))
  (assert-equal '(1 3) (divide-twos 6))
  (assert-equal '(0 7) (divide-twos 7))
  (assert-equal '(3 1) (divide-twos 8))
  (assert-equal '(0 9) (divide-twos 9))
  (assert-equal '(1 5) (divide-twos 10))
  (assert-equal '(0 11) (divide-twos 11))
  (assert-equal '(2 3) (divide-twos 12)))

(define-test lucas-u
  (assert-equal '(0 1 1 2 3 5 8 13 21 34)
    (loop for i from 0 to 9 collect (funcall (lucas-u 1 -1) i)))
  (assert-equal '(0 1 2 5 12 29 70 169 408 985)
    (loop for i from 0 to 9 collect (funcall (lucas-u 2 -1) i))))

(define-test fast-lucas
  (assert-equal '((0 2) (1 1) (1 3) (2 4) (3 7) (5 11) (8 18) (13 29) (21 47) (34 76))
    (loop for i from 0 to 9 collect (fast-lucas i 1 -1)))
  (assert-equal '((0 2) (1 2) (2 6) (5 14) (12 34) (29 82) (70 198) (169 478) (408 1154) (985 2786))
    (loop for i from 0 to 9 collect (fast-lucas i 2 -1)))
  (assert-equal '((0 2) (1 1) (1 5) (3 7) (5 17) (11 31) (21 65) (43 127) (85 257) (171 511))
    (loop for i from 0 to 9 collect (fast-lucas i 1 -2))))

(define-test mod-lucas
  (assert-equal '((0 2) (1 1) (1 3) (2 4) (3 7) (5 0) (8 7) (2 7) (10 3) (1 10))
    (loop for i from 0 to 9 collect (mod-lucas i 1 -1 11)))
  (assert-equal '((0 2) (1 2) (2 6) (5 3) (1 1) (7 5) (4 0) (4 5) (1 10) (6 3))
    (loop for i from 0 to 9 collect (mod-lucas i 2 -1 11)))
  (assert-equal '((0 2) (1 1) (1 5) (3 7) (5 6) (0 9) (10 10) (10 6) (8 4) (6 5))
    (loop for i from 0 to 9 collect (mod-lucas i 1 -2 11))))

(defparameter *jacobi-table*
  (list2hash '(
    1	#(1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1)
    3	#(1	-1	0	1	-1	0	1	-1	0	1	-1	0	1	-1	0	1	-1	0	1	-1	0	1	-1	0	1	-1	0	1	-1	0)
    5	#(1	-1	-1	1	0	1	-1	-1	1	0	1	-1	-1	1	0	1	-1	-1	1	0	1	-1	-1	1	0	1	-1	-1	1	0)
    7	#(1	1	-1	1	-1	-1	0	1	1	-1	1	-1	-1	0	1	1	-1	1	-1	-1	0	1	1	-1	1	-1	-1	0	1	1)
    9	#(1	1	0	1	1	0	1	1	0	1	1	0	1	1	0	1	1	0	1	1	0	1	1	0	1	1	0	1	1	0)
    11	#(1	-1	1	1	1	-1	-1	-1	1	-1	0	1	-1	1	1	1	-1	-1	-1	1	-1	0	1	-1	1	1	1	-1	-1	-1)
    13	#(1	-1	1	1	-1	-1	-1	-1	1	1	-1	1	0	1	-1	1	1	-1	-1	-1	-1	1	1	-1	1	0	1	-1	1	1)
    15	#(1	1	0	1	0	0	-1	1	0	0	-1	0	-1	-1	0	1	1	0	1	0	0	-1	1	0	0	-1	0	-1	-1	0)
    17	#(1	1	-1	1	-1	-1	-1	1	1	-1	-1	-1	1	-1	1	1	0	1	1	-1	1	-1	-1	-1	1	1	-1	-1	-1	1)
    19	#(1	-1	-1	1	1	1	1	-1	1	-1	1	-1	-1	-1	-1	1	1	-1	0	1	-1	-1	1	1	1	1	-1	1	-1	1)
    21	#(1	-1	0	1	1	0	0	-1	0	-1	-1	0	-1	0	0	1	1	0	-1	1	0	1	-1	0	1	1	0	0	-1	0)
    23	#(1	1	1	1	-1	1	-1	1	1	-1	-1	1	1	-1	-1	1	-1	1	-1	-1	-1	-1	0	1	1	1	1	-1	1	-1)
    25	#(1	1	1	1	0	1	1	1	1	0	1	1	1	1	0	1	1	1	1	0	1	1	1	1	0	1	1	1	1	0)
    27	#(1	-1	0	1	-1	0	1	-1	0	1	-1	0	1	-1	0	1	-1	0	1	-1	0	1	-1	0	1	-1	0	1	-1	0)
    29	#(1	-1	-1	1	1	1	1	-1	1	-1	-1	-1	1	-1	-1	1	-1	-1	-1	1	-1	1	1	1	1	-1	-1	1	0	1)
    31	#(1	1	-1	1	1	-1	1	1	1	1	-1	-1	-1	1	-1	1	-1	1	1	1	-1	-1	-1	-1	1	-1	-1	1	-1	-1)
    33	#(1	1	0	1	-1	0	-1	1	0	-1	0	0	-1	-1	0	1	1	0	-1	-1	0	0	-1	0	1	-1	0	-1	1	0)
    35	#(1	-1	1	1	0	-1	0	-1	1	0	1	1	1	0	0	1	1	-1	-1	0	0	-1	-1	-1	0	-1	1	0	1	0)
    37	#(1	-1	1	1	-1	-1	1	-1	1	1	1	1	-1	-1	-1	1	-1	-1	-1	-1	1	-1	-1	-1	1	1	1	1	-1	1)
    39	#(1	1	0	1	1	0	-1	1	0	1	1	0	0	-1	0	1	-1	0	-1	1	0	1	-1	0	1	0	0	-1	-1	0)
    41	#(1	1	-1	1	1	-1	-1	1	1	1	-1	-1	-1	-1	-1	1	-1	1	-1	1	1	-1	1	-1	1	-1	-1	-1	-1	-1)
    43	#(1	-1	-1	1	-1	1	-1	-1	1	1	1	-1	1	1	1	1	1	-1	-1	-1	1	-1	1	1	1	-1	-1	-1	-1	-1)
    45	#(1	-1	0	1	0	0	-1	-1	0	0	1	0	-1	1	0	1	-1	0	1	0	0	-1	-1	0	0	1	0	-1	1	0)
    47	#(1	1	1	1	-1	1	1	1	1	-1	-1	1	-1	1	-1	1	1	1	-1	-1	1	-1	-1	1	1	-1	1	1	-1	-1)
    49	#(1	1	1	1	1	1	0	1	1	1	1	1	1	0	1	1	1	1	1	1	0	1	1	1	1	1	1	0	1	1)
    51	#(1	-1	0	1	1	0	-1	-1	0	-1	1	0	1	1	0	1	0	0	1	1	0	-1	1	0	1	-1	0	-1	1	0)
    53	#(1	-1	-1	1	-1	1	1	-1	1	1	1	-1	1	-1	1	1	1	-1	-1	-1	-1	-1	-1	1	1	-1	-1	1	1	-1)
    55	#(1	1	-1	1	0	-1	1	1	1	0	0	-1	1	1	0	1	1	1	-1	0	-1	0	-1	-1	0	1	-1	1	-1	0)
    57	#(1	1	0	1	-1	0	1	1	0	-1	-1	0	-1	1	0	1	-1	0	0	-1	0	-1	-1	0	1	-1	0	1	1	0)
    59	#(1	-1	1	1	1	-1	1	-1	1	-1	-1	1	-1	-1	1	1	1	-1	1	1	1	1	-1	-1	1	1	1	1	1	-1))))

(define-test jacobi
  (loop for n from 1 to 59 when (oddp n) do
    (loop for k from 1 to 30 do
      (assert-equal (elt (gethash n *jacobi-table*) (- k 1)) (jacobi k n)))))


(defparameter *primes-under-100*
  '(2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97))

(defparameter *primes-under-1000*
  '(2	3	5	7	11	13	17	19	23	29
    31	37	41	43	47	53	59	61	67	71
    73	79	83	89	97	101	103	107	109	113
    127	131	137	139	149	151	157	163	167	173
    179	181	191	193	197	199	211	223	227	229
    233	239	241	251	257	263	269	271	277	281
    283	293	307	311	313	317	331	337	347	349
    353	359	367	373	379	383	389	397	401	409
    419	421	431	433	439	443	449	457	461	463
    467	479	487	491	499	503	509	521	523	541
    547	557	563	569	571	577	587	593	599	601
    607	613	617	619	631	641	643	647	653	659
    661	673	677	683	691	701	709	719	727	733
    739	743	751	757	761	769	773	787	797	809
    811	821	823	827	829	839	853	857	859	863
    877	881	883	887	907	911	919	929	937	941
    947	953	967	971	977	983	991	997))


(define-test prime-sieve
  (assert-equal *primes-under-100* (prime-sieve 100))
  (assert-equal *primes-under-1000* (prime-sieve 1000)))

(defparameter *perfect-squares*
  '(1 4 9 16 25 36 49 64 81 100))

(define-test perfect-square?
  (loop for n from 1 to 100 do
    (if (find n *perfect-squares*)
        (assert-true (perfect-square? n))
        (assert-false (perfect-square? n)))))

(define-test find-lucas-d
  (assert-equal '(nil 5 -7 5 nil 13 5 nil 5 -7 nil 5 nil 5 -11 -7 5 nil 5 nil -7 5 nil 5 nil nil 5 nil 5 -7 -7 5 nil 5 -7 13 5 nil 5 -11 nil 5 nil 5 -7 nil 5 nil 5 nil)
    (loop for n from 1 to 100 by 2 collect (find-lucas-d n))))

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

; The list at https://en.wikipedia.org/wiki/Lucas_pseudoprime#Lucas_probable_primes_and_pseudoprimes
(defparameter *lucas-pseudoprimes-pq*
  (list2hash '(
    (3 -1) (119 649 1189 1763 3599 4187 5559 6681 12095 12403 12685 12871 14041 14279 15051 16109 19043 22847 23479 24769 26795 28421))))

(defparameter *primes-under-30000* (prime-sieve 30000))

(define-test lucas-prime-pq?
  (loop for p from 3 to 3 do
    (loop for q from -1 to -1 do
      (loop for n from 2 to 30000 when (oddp n) do
        (if (coprime n q)
          (if (or (find n *primes-under-30000*)
                  (find n (gethash (list p q) *lucas-pseudoprimes-pq*)))
              (assert-true (lucas-prime-pq? n p q))
              (assert-false (lucas-prime-pq? n p q))))))))

(defparameter *primes-under-10000* (prime-sieve 10000))

(defparameter *lucas-pseudoprimes*
  '(323 377 1159 1829 3827 5459 5777 9071 9179))

(define-test lucas-prime?
  (loop for n from 1 to 10000 do
    (if (or (find n *primes-under-10000*)
            (find n *lucas-pseudoprimes*))
        (assert-true (lucas-prime? n))
        (assert-false (lucas-prime? n)))))

(defparameter *primes-under-20000* (prime-sieve 20000))

(define-test prime?
  (loop for n from 1 to 20000 do
    (if (find n *primes-under-20000*)
        (assert-true (prime? n))
        (assert-false (prime? n)))))

(define-test random-size
  (assert-equal 300 (length (write-to-string (random-size 300)))))

(define-test make-rsa-nums
  (assert-equal '(19 1099 4307) (make-rsa-nums 59 73 19))
  (assert-equal '(4567 103 9797) (make-rsa-nums 97 101 4567)))

(define-test gen-rsa-keys
  (assert-equal 1234567
    (let ((key (gen-rsa-keys 30 5)))
      (decrypt-num (cadr key) (encrypt-num (car key) 1234567))))
  (assert-equal 1234567
    (let ((key (gen-rsa-keys 30 5)))
      (encrypt-num (cadr key) (decrypt-num (car key) 1234567)))))

(defparameter *pub-key*
  '(21799231955134738493
  569103228662024635012191844168676071089499333127436747521717363507952967644548648185316855411013685587748824866944357049185242152788890193054924052451380116147740857630480031371173114047608745476852274327988100920756617372938557897581767029120732719284637445651480475439106864754539046063875699118601745136268285330951625951867887197522594594526634811505942759565928599704092802172070760723609140664678519724612935690143145887153215297005534616481102499843963670975716008785819537914567467929624461949006080055008608003877814430925783137641708100921143935876789848140633154997820901017257587823961248539))

(defparameter *pri-key* 
  '(214503028351579831541136915041590411133042176767248374507472772451033154193902633077197729199068252449927085218393569607594333247697801021572336691395543184204572979573732647877853556969041214118581572521761712986563529023343435033926675758633837121409624197619586603643554740319716667388654691318214052612782461291771349673309763502704613528898850583874333763809075801628297779885884500951687490218229720425776119449384193075374418565981739166424295215550141716801817913470734721302436987540795532498536203408349225854642269891749422466801315359485968228131416466295516545534900715246759605523471010805
  569103228662024635012191844168676071089499333127436747521717363507952967644548648185316855411013685587748824866944357049185242152788890193054924052451380116147740857630480031371173114047608745476852274327988100920756617372938557897581767029120732719284637445651480475439106864754539046063875699118601745136268285330951625951867887197522594594526634811505942759565928599704092802172070760723609140664678519724612935690143145887153215297005534616481102499843963670975716008785819537914567467929624461949006080055008608003877814430925783137641708100921143935876789848140633154997820901017257587823961248539))

(define-test encrypt-decrypt-num
  (assert-equal 12345 (decrypt-num *pri-key* (encrypt-num *pub-key* 12345)))
  (assert-equal 12345 (encrypt-num *pri-key* (decrypt-num *pub-key* 12345))))

(define-test char-to-3-digit-ascii
  (assert-equal "032" (char-to-3-digit-ascii #\Space))
  (assert-equal "116" (char-to-3-digit-ascii #\t))
  (assert-equal "126" (char-to-3-digit-ascii #\~)))

(define-test encode-to-str
  (assert-equal "" (encode-to-str ""))
  (assert-equal "032" (encode-to-str " "))
  (assert-equal "080114111111102115032097114101032097119101115111109101033033033032076079076122"
    (encode-to-str "Proofs are awesome!!! LOLz")))

(define-test encode
  (assert-equal 0 (encode ""))
  (assert-equal 32 (encode " "))
  (assert-equal 80114111111102115032097114101032097119101115111109101033033033032076079076122
    (encode "Proofs are awesome!!! LOLz")))

(define-test group-by-3
  (assert-equal '("1") (group-by-3 "1"))
  (assert-equal '("12") (group-by-3 "12"))
  (assert-equal '("123") (group-by-3 "123"))
  (assert-equal '("1" "234") (group-by-3 "1234"))
  (assert-equal '("12" "345") (group-by-3 "12345"))
  (assert-equal '("123" "456") (group-by-3 "123456"))
  (assert-equal '("1" "234" "567") (group-by-3 "1234567")))

(define-test decode-from-str
  (assert-equal "" (decode-from-str ""))
  (assert-equal " " (decode-from-str "032"))
  (assert-equal " " (decode-from-str "32"))
  (assert-equal "Proofs are awesome!!! LOLz"
    (decode-from-str "080114111111102115032097114101032097119101115111109101033033033032076079076122"))
  (assert-equal "Proofs are awesome!!! LOLz"
    (decode-from-str "80114111111102115032097114101032097119101115111109101033033033032076079076122")))

(define-test decode
  (assert-equal "" (decode 0))
  (assert-equal " " (decode 32))
  (assert-equal "Proofs are awesome!!! LOLz"
    (decode 80114111111102115032097114101032097119101115111109101033033033032076079076122)))

(define-test encrypt-decrypt
  (assert-equal "" (decrypt *pri-key* (encrypt *pub-key* "")))
  (assert-equal "12345" (decrypt *pri-key* (encrypt *pub-key* "12345")))
  (assert-equal "Proofs are awesome!!! LOLz" (decrypt *pri-key* (encrypt *pub-key* "Proofs are awesome!!! LOLz"))))
