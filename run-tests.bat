sbcl --noinform --load lib/rsa.lisp --load testing/lisp-unit.lisp --load testing/rsa-test.lisp --eval "(in-package :rsa-test)" --eval "(progn (run-tests :all) (sb-ext:exit))"
