(defpackage :rsa-main
  (:use :common-lisp :rsa :unix-options)
  (:export :main))

(in-package :rsa-main)

;; Interface

(defparameter *my-keys* nil)
(defparameter *bob-pubkey* nil)

(defun get-option (options)
  (format t "Choose an option: ")
  (finish-output)
  (let ((option (read-line)))
    (loop while (not (find option options :test 'equal)) do
      (format t "Invalid option ~a.
Choose an option: " option)
      (setq option (read-line)))
    (format t "~%")
    option))

(defun get-valid-nat-num (text)
  (format t "Enter ~a: " text)
  (finish-output)
  (let* ((in (read-line)) (num (parse-integer in)))
    (loop while (or (not num) (< num 1)) do
      (format t "Invalid ~a ~a.
Enter ~a: " text in text)
      (setq in (read-line))
      (setq num (parse-integer in)))
    num))

(defun generate-mode ()
  (let ((prime-size (get-valid-nat-num "prime size"))
        (e-size (get-valid-nat-num "e size")))
    (let ((my-keys (gen-rsa-keys prime-size e-size)))
      (destructuring-bind ((e n) (d nil)) my-keys
        (setq *my-keys* my-keys)
        (format t "~%")
        (format t "Generated public and private keys:~%~%")
        (format t "e: ~a~%" e)
        (format t "d: ~a~%" d)
        (format t "n: ~a~%" n)
        (format t "~%")
        (main-menu)))))

(defun main ()
  (with-cli-options (sb-ext:*posix-argv*)
      (generate encrypt decrypt out keyfile message)
    (format t "generate: ~S encrypt: ~S decrypt: ~S out: ~S keyfile: ~S message: ~S~%~%"
      generate encrypt decrypt out keyfile message))
  (format t "ECE 103 RSA Program~%~%")
  (main-menu))

(defun main-menu ()
  (format t "1. Generate keys
2. Import keys
3. Encrypt message
4. Exit

")
  (let ((option (parse-integer (get-option '("1" "2" "3" "4")))))
    (case option (1 (generate-mode))
                 (2 (import-mode))
                 (3 (encrypt-mode)))))