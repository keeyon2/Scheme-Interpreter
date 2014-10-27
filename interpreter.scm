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
      (else 
        (cadr item))
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

(define (my-evalPrints exp env)
  (newline)
  (display "Calling my-eval on exp: ")
  (display exp)
  (newline)

  (newline)
  (display "The env of this call is")
  (newline)
  (displayEnv env)
  (newline)

  (cond ((pair? exp) 
  (display "(car exp) of that exp: ")
  (display (car exp))))
  (newline)

  (cond
   ((symbol? exp) (display "Hit symbol?") (lookup exp env))
   ((not (pair? exp)) (display "Hit not pair?") exp)
   ((eq? (car exp) 'quote) (display "Hit 'qoute") (cadr exp))
   ((eq? (car exp) 'if) (display "Hit if")
    (handle-if (cadr exp) (caddr exp) (cadddr exp) env))
   ((eq? (car exp) 'lambda) (display "Hit lambda")
    (list 'closure exp env))
   ((eq? (car exp) 'let) (display "Hit let")
    (handle-let (cadr exp) (cddr exp) env))
   ((eq? (car exp) 'letrec)  (display "Hit letrec")
    (handle-letrec (cadr exp) (cddr exp) env))  ;; see explanation below
   (else
     (display "Calling handle call")
    (handle-call (map (lambda (sub-exp) (my-eval sub-exp env)) exp)))
   ))

(define (my-eval exp env)
  (cond
   ((symbol? exp) (lookup exp env))
   ((not (pair? exp)) exp)
   ((eq? (car exp) 'quote) (cadr exp))
   ((eq? (car exp) 'if) 
    (handle-if (cadr exp) (caddr exp) (cadddr exp) env))
   ((eq? (car exp) 'lambda) 
    (list 'closure exp env))
   ((eq? (car exp) 'let) 
    (handle-let (cadr exp) (cddr exp) env))
   ((eq? (car exp) 'let*) 
    (handle-let* (cadr exp) (cddr exp) env))
   ((eq? (car exp) 'letrec)  
    (handle-letrec (cadr exp) (cddr exp) env))  ;; see explanation below
   (else
    (handle-call (map (lambda (sub-exp) (my-eval sub-exp env)) exp)))
   ))

(define (bind formals actuals)
  (display "We are in bind")(newline)
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

(define (handle-let defs body env)
  ;; return nothing if body empty
  ;; (display "We are in handle-let")(newline)
  ;; (display "The defs are: ")(newline)
  ;; (displayEnv defs)
  ;; (newline)(display "And the body is: ")(newline)
  ;; (displayEnv body)
  ;; (pair? defs) - recal function with no defs and new environment
  (cond ((pair? defs) (handle-let '() body (handle-let-defs-to-env defs env env)))
	    ((null? defs) ;; (display "We now have the env correct") (newline) 
                      ;; (displayEnv env)
                      ;; (display "Calling (my-eval body env)")(newline)
                      ;; (display "body: ")(display body)(newline)
                      ;; (display "(car body): ")(display (car body))(newline)
                      (cond ((pair? body) 
                       ;; (display "body: ")(display body)(newline)
                       ;; (display "(car body): ")(display (car body))(newline)
   		  		        (my-eval (car body) env)
                        (handle-let defs (cdr body) env))
                            )
			    )))	 	 


;; Takes all the def's and returns 
;; Should initially  pass in (Defs, envOrig, envOrig)
(define (handle-let-defs-to-env defs envOrig envNew)
  (cond ((null? defs) envNew)
    ((pair? defs) 
        (handle-let-defs-to-env (cdr defs) envOrig (cons (list (car (car defs)) (my-eval (cadr (car defs)) envOrig)) envNew))
	)))
		
(define (handle-let* defs body env)
  (cond ((pair? defs) 
         (handle-let* (cdr defs) body (cons (list (car (car defs)) (my-eval (cadr (car defs)) env)) env)))
         ((null? defs)
          (cond ((pair? body)
                 (my-eval (car body) env)
                 (handle-let* defs (cdr body) env))
                )
          )))

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
    (display "\n"))

(define (displayEnv env)
  (cond ((null? env))
	(else 
        (display (car env))
	    (newline)
	    (displayEnv (cdr env)))
	))
	
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
    (list 'newline (list 'primitive-function newline2))
    (list 'displayEnv (list 'primitive-function displayEnv))
    ))
