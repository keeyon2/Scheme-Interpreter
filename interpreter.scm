                            ;;;Mini-Scheme Interpreter


;;; Your first task is to understand this. 

(define (repl)     ;;; the read-eval-print loop.
  (display "--> ") 
  (let ((exp (read)))
    (cond ((equal? exp '(exit))      ; (exit) only allowed at top level
       'done)
      (else  (display (top-eval exp))
         (newline)
         (repl))
      )))

;; Use this to test shit later
(define testExecution (lambda () (+ (4 * 1)(2 * 9))))

(define (testEx) 
  (+ (4 * 1)(2 * 9))
  )


(define (my-load filename)       ;; don't want to redefine the Scheme LOAD
  (load-repl (open-input-file filename)))


(define (load-repl port)
  (let ((exp (read port)))
    (cond ((eof-object? exp) 'done)
      (else (let ((res (top-eval exp)))
          (display res)
          (load-repl port)))
      )))



;; insert!, below, is a destructive update of a list L, inserting the
;; parameter val onto the front of L (so that L is actually modified).
;; insert! must only be used where absolutely necessary, e.g. when an
;; environment must be destructively updated to allow for recursion
;; (see the implementation of (define ...) below).

;; As their names imply, set-car! and set-cdr! destructively modify 
;; the car field and cdr field of a cons cell, respectively. They are
;; built-in functions (see *global-env* below).

(define (insert! val L)
  (set-cdr! L (cons (car L) (cdr L)))
  (set-car! L val)
  )


;; (define ....) is only allowed at the top level and affects only the 
;; global environment. Only the basic form of define is supported here.

(define (top-eval exp)
  ;(display "Top-eval exp: ")
  ;(display exp)
  ;(newline)
  (cond ((not (pair? exp)) (my-eval exp *global-env*))
    ((eq? (car exp) 'define)   
     (insert! (list (cadr exp) (my-eval (caddr exp) *global-env*)) *global-env*)
     (cadr exp)) ; just return the symbol being defined
    (else (my-eval exp *global-env*))
    ))


(define (lookup var env)
  (let ((item (assoc var env)))  ;; assoc returns #f if var not found in env
    (cond ((not item) (display "Error: Undefined Symbol ")
              (display var) (newline))
      (else (cadr item))
      )))

(define (handle-if test then-exp else-exp env)
  (if (my-eval test env)
      (my-eval then-exp env)
      (my-eval else-exp env)))


;; still missing let, let*, letrec, the syntax for (define (f x) ...),
;; cond, begin (block).

(define (my-evalOriginal exp env)
  (cond
   ((symbol? exp) (lookup exp env))
   ((not (pair? exp))  exp)
   ((eq? (car exp) 'quote)  (cadr exp))
   ((eq? (car exp) 'if) 
    (handle-if (cadr exp) (caddr exp) (cadddr exp) env))
   ((eq? (car exp) 'lambda) 
    (list 'closure exp env))
   ((eq? (car exp) 'letrec)     
    (handle-letrec (cadr exp) (cddr exp) env))  ;; see explanation below
   (else
     (display "Calling handle call")(newline)
    (handle-call (map (lambda (sub-exp) (my-eval sub-exp env)) exp)))
   ))

(define (my-eval exp env)
  (newline)
  (display "Calling my-eval on exp: ")
  (display exp)
  (newline)
  (cond
   ((symbol? exp) (display "Hit symbol?") (lookup exp env))
   ((not (pair? exp)) (display "Hit not pair?") exp)
   ((eq? (car exp) 'quote) (display "Hit 'qoute") (cadr exp))
   ((eq? (car exp) 'if) (display "Hit if")
    (handle-if (cadr exp) (caddr exp) (cadddr exp) env))
   ((eq? (car exp) 'lambda) (display "Hit lambda")
    (list 'closure exp env))
   ((eq? (car exp) 'letrec)  (display "Hit letrec")
    (handle-letrec (cadr exp) (cddr exp) env))  ;; see explanation below
   (else
     (display "Calling handle call")
    (handle-call (map (lambda (sub-exp) (my-eval sub-exp env)) exp)))
   ))

(define (my-eval3 exp env)
  (cond
   (display "my eval exp: ")
   (display exp)
   (newline)
   ;; This is start of real code
   ((symbol? exp) ((display "Hit symbol? ")(newline)(lookup exp env)))
   ((not (pair? exp)) (display "Hit not pair?")(newline) exp)
   ((eq? (car exp) 'quote) (display "Hit exp='qoute") (cadr exp))
   ((eq? (car exp) 'if) (display "Hit If")(newline)
    (handle-if (cadr exp) (caddr exp) (cadddr exp) env))
   ((eq? (car exp) 'lambda) (display "Hit Lambda")(newline)
    (list 'closure exp env))
   ((eq? (car exp) 'letrec) (display "Hit Letrec")(newline)
    (handle-letrec (cadr exp) (cddr exp) env))  ;; see explanation below
   (else
     (display "Calling handle call")(newline)
    (handle-call (map (lambda (sub-exp) (my-eval3 sub-exp env)) exp)))
   ))


;; Still need to do some shit
(define (my-eval2 exp env)
  ;(display "my-eval exp: ")
  ;(display exp)
  ;(newline)
  (cond
   ((symbol? exp) (lookup exp env) ;(display "SymbolCalled: ") (display exp) 
    (newline))
   ((not (pair? exp)) exp)
   ((eq? (car exp) 'quote) (cadr exp))
   ((eq? (car exp) 'if)
    (handle-if (cadr exp) (caddr exp) (cadddr exp) env))
   ((eq? (car exp) 'lambda)
    (list 'closure exp env))
   ((eq? (car exp) 'letrec)
    (handle-letrec (cadr exp) (cddr exp) env))  ;; see explanation below
   (else
    (handle-call (map (lambda (sub-exp) (my-eval2 sub-exp env)) exp)))
   ))


(define (bind formals actuals)
  (cond ((null? formals) '())
    (else (cons (list (car formals) (car actuals))
            (bind (cdr formals) (cdr actuals))))
    ))

(define (handle-block block env)
  (cond ((null? block) (display "Error: Can't have empty block or body"))
    ((null? (cdr block)) (my-eval (car block) env))
    (else (my-eval (car block) env)
          (handle-block (cdr block) env))
    ))
    

; Here's how handle-letrec should implement LETREC
; 0) The parameters are the defs,(e.g. ((f exp1) (g exp2)), and the body,
;    which is a list of expressions, e.g. ((display x) (f (g 1)))
; 1) create an association list binding the new names introducted by
;    the letrec to uninitialized values (e.g. the symbol '*uninitialized*).
;    For example, if the new names are x and y, then create 
;    ((x *uninitialized*) (y *uninitialized*))
; 2) create a new-env by appending the above association list to env.
; 3) eval the right hand side of each def using new-env
; 4) destructively modify new-env to replace the unitialized value for each
;    new name with its correspondinng value.
; 5) evaluate the body of the letrec using new-env


(define (handle-letrec defs body env)
  (display "Error: letrec not implemented yet") 
  (newline))


(define (handle-call evald-exps)
  (let ((fn (car evald-exps))
    (args (cdr evald-exps)))
   (cond
    ((eq? (car fn) 'closure) (display "In handle Call's Closure")(newline)
     (let ((formals (cadr (cadr fn)))
       (body (cddr (cadr fn)))
       (env (caddr fn)))
       (handle-block body (append (bind formals args) env))))
    ((eq? (car fn) 'primitive-function)
     (apply (cadr fn) args))
    (else (display "Error: Calling non-function"))
    )))

  (define (newline2)
    (display "\n")
    (display "\n")
    (display "\n")
    (display "\n"))

;;-------------------- Here is the initial global environment --------

(define *global-env*
  (list 
    (list 'car (list 'primitive-function car))
    (list 'cdr (list 'primitive-function cdr))
    (list 'set-car! (list 'primitive-function set-car!))
    (list 'set-cdr! (list 'primitive-function set-cdr!))
    (list 'cons (list 'primitive-function cons))
    (list 'list (list 'primitive-function list))
    (list '+ (list 'primitive-function +))
    (list '- (list 'primitive-function -))
    (list '* (list 'primitive-function *))
    (list '= (list 'primitive-function =))
    (list '< (list 'primitive-function <))
    (list '> (list 'primitive-function >))
    (list '<= (list 'primitive-function  <=))
    (list '>= (list 'primitive-function >=))
    (list 'eq? (list 'primitive-function eq?))
    (list 'pair? (list 'primitive-function pair?))
    (list 'symbol? (list 'primitive-function symbol?))
    (list 'null? (list 'primitive-function null?))
    (list 'read (list 'primitive-function read))
    (list 'display (list 'primitive-function  display))
    (list 'open-input-file (list 'primitive-function open-input-file))
    (list 'close-input-port (list 'primitive-function close-input-port))
    (list 'eof-object? (list 'primitive-function eof-object?))
    (list 'load (list 'primitive-function my-load))  ;;defined above
    (list 'newline2 (list 'primitive-function newline2))
    ))
