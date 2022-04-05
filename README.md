# A Synthesis Tool for Optimal Monitors in a Branching-Time Setting

A prototype tool using OCaml that generates the strongest monitorable consequence from arbitrary branching-time rechHML formula.

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
#### Libraries 
PrintBox is a library used to print nested boxes, lists, arrays, tables in several formats.
```
opam install printbox printbox-text
eval $(opam env)
```
#### To generates a build system, produce the files setup.ml, configure and Makefile, along with some others which can be safely ignored
```
oasis setup -setup-update dynamic
```
#### To build the project
```
cd Tool
make
```

## The Tool 

A short demo video for this tool can be found on [YouTube](https://youtu.be/XI6GoG4MaNk). 
<!-- We highly encourage you to consult the paper that sets all the theoretical foundations for this tool.   -->

## Assumptions
This tool relies on a number of assumptions, which are listed below.
* All recursion variables are bound by least or greatest fixed points (i.e., formulas are closed).
* In case of syntax errors, no attempt at solving them is made. Instead, a parsing error is returned. 
* The formula is already in disjunctive form. If not, the tool still attempts to generate the strongest monitorable consequence but with no guarantee that the final result will be its best monitorable approximation.  

## Running the tool
There are two ways how this tool can be employed to generate the strongest monitorable consequence of a branching-time recHML property. 

#### Option 1: Passing the formula as a command line argument 
Run the native code and pass the formula as a command line argument. The formula below is solely for demonstrative purposes and should be changed accordingly.
```
./main.native "([a]ff & [b]ff) | ([a]ff & [c]ff)"
```

#### Option 2: Specifying the location of a file containing the formula 
Run the native code.
```
./main.native
```
When prompted, enter the full file path.

<!-- ## Authors
