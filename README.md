# scheme-lib
My personal library of Scheme functions, free for anyone to borrow. Nearly all of the code is based on (or copied from) 
the Structure and Interpretation of Programming Languages by Harold Abelson and Gerald Jay Sussman
with Julie Sussman. https://web.mit.edu/alexmv/6.037/sicp.pdf. 

## Getting Started
- Install MIT Scheme `brew install mit-scheme`
- Clone this repo `git clone https://github.com/preston-evans98/scheme-lib.git`
- Insert `(load "path/to/scheme-lib/math.scm")` at the top of your custom scheme file. This will make every function available to your program from `math.scm` and `utils.scm` available to your program
- Run your Scheme file `scheme --quiet < myfile.scm` 

## Library
### Utils.scm
A few bare bones utilites

### Math.scm (depends on "utils.scm")
The core of the library. Pretty stable and reasonably organized

## Bonus
##### Chapter2.scm (depends on "math.scm", "utils.scm")
Selected exercises from SCIP chapter 2 - https://web.mit.edu/alexmv/6.037/sicp.pdf
Caveat lector. This file is a jumble of solutions to some exercises from chapter 2. It is NOT
optimized for readability or ease of use, but is provided here in case it can be helpful

##### Chapter3.scm (depends on "math.scm", "utils.scm")
Selected exercises from SCIP chapter 3
This file is a work in progress... Caveat lector. It's a jumble of solutions to some exercises from chapter 3 and is NOT
optimized for readability or ease of use. It is provided here in case it can be helpful
