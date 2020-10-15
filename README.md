Say hello to Diamondback!

Diamondback is a functional language that has its own compiler which, parses,
type checks, and creates executable in assemblies.

Diamondback supports memory allocation, garbage collection, functions, 
data structures (pair, trees, list, etc.), object comparisons, mathematical
operations, boolean, loops, if/else statement (for formatting and syntax please 
refer to Syntax.png). Supports 64 bits integer. 

To compile and run : 

Requirements for running the codes are OCaml, OPAM, and running the code
$ opam install extlib ounit ocamlfind
on a terminal.

Let example.boa be a file with code in the Diamondback language. To view
assembly code for language in Diamondback, run :

$ make main
$ ./main input/example.boa

To actually run assembly code :

Create the assembly file
$ make output/example.s

Create the executable
$ make output/example.run

Run the file
$ ./output/example.run