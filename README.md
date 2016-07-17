# Common Lisp RSA System

## Compile

`./compile`

## Generate public-private key pair

`./rsa -g <key file> -p 300 -e 20`

This will make two files: `<key file>` and `<key file>.pub`

The format of the public key is `(e n)` and the format of the private key is `(d n)`

## Encrypt

`./rsa --encrypt "<message>" -k <public key file>`

## Decrypt

`./rsa --decrypt "<numerical message>" -k <private key file>`

## ASCII Encode only

`./rsa --encode "<message>"`

## ASCII Decode only

`./rsa --decode "<ascii message>"`

## Decrypt without decode

`./rsa --decrypt-no-decode "<numerical message>" -k <private key file>`

## Run tests

`./run-tests`

## Open REPL with everything loaded

`./repl`
