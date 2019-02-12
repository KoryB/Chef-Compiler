# Chef-Compiler
Partial compiler for the [chef](http://www.dangermouse.net/esoteric/chef.html) programming language, completed in my fourth year at college.

## Running
Simply use:

    $ racket main.rkt 
    
to compile the chef program in exFinal/input1.txt. To specify a custom input file the format is:

    $ racket main.rkt exFinal/grammar.txt <input file>
    
note that a custom grammar file could be used in place of "exFinal/grammar.txt", however the one provided is a working copy for the chef programming language; modified somewhat to support the compilation process.

## Output
After compiling a chef text file, to outputs are generated: tree.dot and chef.asm. "tree.dot" is a dot file representing a visualization of the grammar tree, and chef.asm is the assembly file for the given input. Transformation from assembly into an executable is beyond the scope of this project, although we used nasm and gcc in class.

Optionally, one can uncomment certain lines in main.rkt in order to output the parse table used for the compilation process, among other things.

## Limitations
Currently the chef compiler only supports the language features in the two example inputs given in exFinal/. More can be added fairly easily by modifying assemblizer.rkt, but there are no plans to do so at the moment.
