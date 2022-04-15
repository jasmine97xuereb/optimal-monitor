# A Synthesis Tool for Optimal Monitors in a Branching-Time Setting

A prototype tool using OCaml that generates the strongest monitorable consequence from arbitrary branching-time rechHML formulas.

## Getting Started

These instructions will get you a copy of the project up and running on your local machine for development and testing purposes. 
We assume that users are running Ubuntu/Debian or macOS. 
For Windows 10, we suggest installing the Windows Subsystem for Linux and following the instructions below.
 <!-- or alternatively, configure Chocolatey and install the software packages mentioned via choco install. However, our instructions will not detail how detectEr is installed and used on Windows systems. -->
Before following the rest of the instructions, please install the software package management system available for your particular operating system.

### Installing

For Ubuntu/Debian, we recommend using the [APT](https://ubuntu.com/server/docs/package-management) package manager, which comes pre-bundled.
For MacOS, we recommend using Homebrew. For installation, follow the guide provided [here](https://brew.sh).

<!-- #### Installs curl
For Ubuntu/Debian, install curl using APT.
```
sudo apt install curl
```
For MacOS, install curl using Homebrew.
```
brew install curl
``` -->
#### Install opam 
<!-- The following command simply downloads and installs the proper pre-compiled binary for Opam based on your architecture. -->
Opam is the package manager for OCaml. 
<!-- ```
sh <(curl -sL https://raw.githubusercontent.com/ocaml/opam/master/shell/install.sh)
``` -->
For Ubuntu/Debian, install opam using APT.
```
sudo apt install opam
``` 
For MacOS, install opam using Homebrew.
```
brew install opam
```
#### Set up environment
In this step, you will be asked whether you want your zsh configuration to be updated. We suggest denying and setting up the necessary environment variables manually each time (as demonstrated in what follows).     
```
opam init
```
#### Set up necessary environment variables
```
eval $(opam env)
```
#### Install the OCaml compiler version 4.08.0
```
opam switch create 4.08.0
eval $(opam env)
```
#### Install opam-depext 
Opam-depext is a tool to query and install external dependencies of opam packages. 
If you are running opam version 2.1 or later, skip this step as it is installed automatically.   
```
opam install depext
eval $(opam env)
```
#### Install Zarith 
Zarith is a library used to implement arithmetic and logical operations over arbitrary-precision integers.
```
opam install zarith
eval $(opam env)
```
#### Install Menhir 
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
CSV is a library to read and write CSV files. 
```
opam install printbox printbox-text
opam install csv
eval $(opam env)
```
#### To generate a build system, produce the files setup.ml, configure and Makefile, along with some others which can be safely ignored
```
cd Tool
oasis setup -setup-update dynamic
```
#### To build the project
```
make
```

### Installing (for Evaluation)
Here, we guide the reader on how the environment should be set up for performing the evaluation. 
For Ubuntu/Debian, install python and other required packages using APT. 
For MacOS, use Homebrew (by replacing *sudo apt-get* by *brew*). 
```
sudo apt install python3
sudo apt install python3-numpy
sudo apt install python3-pandas
```

## The Tool 

A short demo video for this tool can be found on [YouTube](https://youtu.be/XI6GoG4MaNk). 
<!-- We highly encourage you to consult the paper that sets all the theoretical foundations for this tool.   -->

### Assumptions
This tool relies on a number of assumptions, which are listed below.
* All recursion variables are bound by least or greatest fixed points (i.e., formulas are closed).
* In case of syntax errors, no attempt at solving them is made. Instead, a parsing error is returned. 
* The formula is already in disjunctive form. If not, the tool still attempts to generate the strongest monitorable consequence but with no guarantee that the final result will be its best monitorable approximation.  

### Running the tool
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
