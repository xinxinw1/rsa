# Common Lisp RSA System

## Compile

1. `./compile`

## Generate public-private key pair

1. `./rsa -g <key file> -p 300 -e 20`

This will make two files: `<key file>` and `<key file>.pub`

The format of the public key is `(e n)` and the format of the private key is `(d n)`

## Encrypt

1. `./rsa --encrypt "<message>" -k <public key file>`

## Decrypt

1. `./rsa --decrypt "<numerical message>" -k <private key file>`

## ASCII Encode only

1. `./rsa --encode "<message>"`

## ASCII Decode only

1. `./rsa --decode "<ascii message>"`

## Decrypt without decode

1. `./rsa --decrypt-no-decode "<numerical message>" -k <private key file>`

## Run tests

1. `./run-tests`

## Open REPL with everything loaded

1. `./repl`
