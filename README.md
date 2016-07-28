# Common Lisp RSA System

The code that does all the computation is in `lib/rsa.lisp` which only uses the Common Lisp Standard Library. `interface/rsa-main.lisp` is a command line interface to my RSA library and uses the open-source `unix-options` library to parse command line options. `testing/rsa-test.lisp` contains unit tests and uses the open-source `lisp-unit` library.

## Dependencies

The command line interface and shell scripts currently only works with the Common Lisp distribution SBCL (Steel Bank Common Lisp) on Linux.

## Download

Install [Steel Bank Common Lisp](http://www.sbcl.org/).

`git clone https://github.com/xinxinw1/rsa.git`  
`cd rsa`

## Compile

`./compile`

After compiling:

### Generate public-private key pair

`./rsa -g <key file> -p 300 -e 20`

This will make two files: `<key file>` and `<key file>.pub`

`-p 300` specifies the size/number of digits of one of the primes generated (the other one has this size + 3 to make factoring harder). `-e 20` specifies the size of the public key constant `e`.

The format of the public key is `(e n)` and the format of the private key is `(d n)`

### Encrypt

`./rsa --encrypt $'<message>' -k <public key file>`

### Decrypt

`./rsa --decrypt <numerical message> -k <private key file>`

### ASCII Encode only

`./rsa --encode $'<message>'`

### ASCII Decode only

`./rsa --decode <ascii message>`

### Encrypt without encode

`./rsa --encrypt-no-encode <numerical message> -k <public key file>`

### Decrypt without decode

`./rsa --decrypt-no-decode <numerical message> -k <private key file>`

## Run tests

`./run-tests`

## Open Lisp REPL (Read-eval-print loop) with everything loaded

`./repl`
