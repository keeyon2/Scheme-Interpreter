Scheme-Interpreter
==================

Scheme Repl and Interpreter that Interprets itself, or as I call it, SchemeCeption

I redefined all built in function scheme uses with definitions built with the most 
basic Scheme functions. Mostly car, cdr, cons

Redefined Functions:
- load
- define
- lambda interpretation
- assoc
- insert!
- let
- let*
- letrec
- apply
- append
- map
- and
- or
- not
- equal?
- cadr
- cddr
- cdddr
- caddr
- caddd


Installing Scheme
------------------------------

- This scheme interpreter uses the Racket implementation of Scheme. This can be found at: http://racket-lang.org/

- If you want to use the Racket development environment, be sure to select "R5RS" as the language.

- To use a command-line version in shell, run the executable plt-r5rs, which is found in the bin/ directory within the Racket folder that you will have downloaded. 
I created an alias to it named scheme, so I just type scheme to run the system.

Running Scheme Interpreter
------------------------------

This tests the library 
> (load "interpreter.scm")

> (repl)

> --> (load "library.scm")

...

> --> (load "test_library.scm")

...

> --> (test-library)  


This gets the interpreter to interpret itself
> (load "interpreter.scm")

> (repl)

> --> (load "library.scm")

> --> (load "interpreter.scm")

> --> (repl)

> --> (define (fac x) (if (= x 0) 1 (* x (fac (- x 1)))))

> --> (fac 4)

> --> 24

> --> (exit)

> --> (exit)

>                                ; back in scheme system

