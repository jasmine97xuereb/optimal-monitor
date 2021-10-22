# The Best a Monitor can Do

A tool using OCaml that generates the strongest monitorable consequence from an arbitrary rechHML formula.

## Getting Started

These instructions will get you a copy of the project up and running on your local machine for development and testing purposes. 

### Installing

What things you need to install the software and how to install them.

#### Installs curl
```
sudo apt install curl
```

#### Installs opam 
Opam is the package manager for OCaml.
```
sh <(curl -sL https://raw.githubusercontent.com/ocaml/opam/master/shell/install.sh)
```
#### Sets up environment
```
opam init
```
#### Sets up necessary environment variables
```
eval $(opam env)
```
#### Installs the OCaml compiler version 4.08.0
```
opam switch create 4.08.0
eval $(opam env)
```
#### Installs opam-depext 
Opam-depext is a tool to query and install external dependencies of opam packages.
```
opam install depext
eval $(opam env)
```
#### Installs Zarith 
Zarith is a library used to implement arithmetic and logical operations over arbitrary-precision integers.
```
opam install zarith
eval $(opam env)
```
#### Installs Menhir 
Menhir is a parser generator for OCaml.
```
opam install menhir
eval $(opam env)
```
#### Install Oasis 
Oasis is a tool used to integrate a configure, build and install system in for OCaml projects.
```
opam install oasis
eval $(opam env)
```
#### To generates a build system, produce the files setup.ml, configure and Makefile, along with some others which can be safely ignored
```
oasis setup -setup-update dynamic
```
#### To build the project
```
make
```


<!-- ## Authors