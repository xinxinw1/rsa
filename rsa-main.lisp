(defpackage :rsa-main
  (:use :common-lisp :rsa :unix-options)
  (:export :main))

(in-package :rsa-main)

;; Interface

(defparameter *my-keys* nil)
(defparameter *bob-pubkey* nil)

(defun save-key-in-file (key file)
  (with-open-file (fstream file :direction :output)
    (format fstream "~S~%" key)))

(defun read-file (file)
  (with-open-file (fstream file)
    (read fstream)))

(defun gen-key-to-file (prime-size e-size file)
  (destructuring-bind (pub pri) (gen-rsa-keys prime-size e-size)
    (save-key-in-file pri file)
    (save-key-in-file pub (concatenate 'string file ".pub"))))

(defun encode-message (message)
  (format t "~a~%" (encode message)))

(defun decode-message (message)
  (format t "~a~%" (decode (parse-integer message))))

(defun encrypt-message (message file)
  (format t "~a~%" (encrypt (read-file file) (encode message))))

(defun decrypt-message (message file)
  (format t "~a~%" (decode (decrypt (read-file file) (parse-integer message)))))

(defun encrypt-no-encode (message file)
  (format t "~a~%" (encrypt (read-file file) (parse-integer message))))

(defun decrypt-no-decode (message file)
  (format t "~a~%" (decrypt (read-file file) (parse-integer message))))

(defun main ()
  (with-cli-options (sb-ext:*posix-argv*)
      (&parameters generate p-size e-size encrypt encode encrypt-no-encode decrypt decode decrypt-no-decode keyfile)
    (cond (generate (gen-key-to-file (parse-integer p-size) (parse-integer e-size) generate))
          (encode (encode-message encode))
          (decode (decode-message decode))
          (encrypt (encrypt-message encrypt keyfile))
          (decrypt (decrypt-message decrypt keyfile))
          (encrypt-no-encode (encrypt-no-encode encrypt-no-encode keyfile))
          (decrypt-no-decode (decrypt-no-decode decrypt-no-decode keyfile)))))
