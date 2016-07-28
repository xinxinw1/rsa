sbcl --noinform --load lib/rsa.lisp --load interface/unix-options.lisp --load interface/rsa-main.lisp --eval "(save-lisp-and-die \"rsa.exe\" :toplevel 'rsa-main:main :executable t)"
