Scheme-Interpreter
==================

Scheme Repl and Interpreter that Interprets itself, or as I call it, SchemeCeption

I redefined all built in function scheme uses with definitions built with the most 
basic Scheme functions. Mostly car, cdr, cons

Redefined Functions:
load,
define,
assoc,
insert!,
let,
let*,
letrec,
apply,
append,
map,
and,
or,
not,
equal?,
cadr,
cddr,
cdddr,
caddr,
cadddr,

Also have functionality to interpreter and handle these calls, and also lambda 

To run the scheme interpreter
Installing and Running Scheme

This scheme interpreter uses the Racket implementation of Scheme. This can be found at: http://racket-lang.org/

If you want to use the Racket development environment, be sure to select "R5RS" as the language.

To use a command-line version in shell, run the executable plt-r5rs, which is found in the bin/ directory within the Racket folder that you will have downloaded. 
I created an alias to it named scheme, so I just type scheme to run the system.

This is how to test the library 
> (load "interpreter.scm")     ;loading interpreter into Scheme system                     
> (repl)
--> (load "library.scm")

...

--> (load "test_library.scm")

...

--> (test-library)  


This is how to get the interpreter to interpret itself
> (load "interpreter.scm")     ;loading interpreter into Scheme system                     

> (repl)                         ;invoking interpreter, note the change in prompt

--> (load "library.scm")         ;load the library file

--> (load "interpreter.scm")      ;loading intrepreter into interpreter

--> (repl)                          ; invoking 2nd interpreter

--> (define (fac x) (if (= x 0) 1 (* x (fac (- x 1)))))  ;defining factorial

--> (fac 4)                                              ;calling factorial

--> 24

--> (exit)                          ; exiting 2nd interpreter

--> (exit)                          ; exiting 1st interpreter

>                                ; back in scheme system

