# Common Lisp RSA System

The code that does all the computation is in `lib/rsa.lisp` which only uses the Common Lisp Standard Library. `interface/rsa-main.lisp` is a command line interface to my RSA library and uses the open-source `unix-options` library to parse command line options. `testing/rsa-test.lisp` contains unit tests and uses the open-source `lisp-unit` library.

## Dependencies

The command line interface and shell scripts currently only works with the Common Lisp distribution SBCL (Steel Bank Common Lisp).

## Download

Install [Steel Bank Common Lisp](http://www.sbcl.org/).

On Linux or if you have git on Windows:

`git clone https://github.com/xinxinw1/rsa.git`  
`cd rsa`

On Windows without git:

Click the green "Clone or download" button on the top right on Github, click "Download ZIP". Extract to some directory. Open Command Prompt and cd to that directory.

## Compile

On Linux: `./compile` This will make a file `rsa` in the current directory.

On Windows: Double click `compile.bat`. This will make a file `rsa.exe` in the current directory.

After compiling:

On Windows: Open Command Promp

### Generate public-private key pair

On Linux: `./rsa -g keys/<key file> -p 300 -e 20`  
On Windows: `rsa -g keys/<key file> -p 300 -e 20`

This will make two files: `keys/<key file>` and `keys/<key file>.pub`

`-p 300` specifies the size/number of digits of one of the primes generated (the other one has this size + 3 to make factoring harder). `-e 20` specifies the size of the public key constant `e`.

The format of the public key is `(e n)` and the format of the private key is `(d n)`

### Encrypt

On Linux: `./rsa --encrypt $'<message>' -k keys/<public key file>`

`$'<message>'` is the Bash string escape syntax so you can include special characters like `!`, `'`, or `"`, for example `$'What\'s that "thing"!!??'`.

On Windows: `rsa --encrypt "<message>" -k keys/<public key file>`

### Decrypt

On Linux: `./rsa --decrypt <numerical message> -k keys/<private key file>`  
On Windows: `rsa --decrypt <numerical message> -k keys/<private key file>`

### ASCII Encode only

On Linux: `./rsa --encode $'<message>'`  
On Windows: `rsa --encode "<message>"`

### ASCII Decode only

On Linux: `./rsa --decode <ascii message>`  
On Windows: `rsa --decode <ascii message>`

### Encrypt without encode

On Linux: `./rsa --encrypt-no-encode <numerical message> -k keys/<public key file>`  
On Windows: `rsa --encrypt-no-encode <numerical message> -k keys/<public key file>`

### Decrypt without decode

On Linux: `./rsa --decrypt-no-decode <numerical message> -k keys/<private key file>`  
On Windows: `rsa --decrypt-no-decode <numerical message> -k keys/<private key file>`

## Run tests

On Linux: `./run-tests`
On Windows: Double click `run-tests.bat`.

## Open Lisp REPL (Read-eval-print loop) with everything loaded

On Linux: `./repl`
On Windows: Double click `repl.bat`.
