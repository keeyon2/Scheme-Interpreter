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


(define (my-load filename)       ;; don't want to redefine the Scheme LOAD
  (load-repl (open-input-file filename)))

(define (load-repl port)
  (let ((exp (read port)))
    (cond ((eof-object? exp) 'done)
      (else (let ((res (top-eval exp)))
          ;;(display res)
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
  ;; Whether or not we have an exp that is a pair or not
  (cond ((not (pair? exp)) (my-eval exp *global-env*))
    ((eq? (car exp) 'define)   
     ;; Define of var
     (cond ((not (pair? (cadr exp))) 
                 (insert! (list (cadr exp) (my-eval (caddr exp) *global-env*)) *global-env*)
                 (cadr exp)) ; just return the symbol being defined

            ;; Take care of function def  //Look in notes
            ((pair? (cadr exp))
              (insert! (list (car (cadr exp)) (my-eval (cons 'lambda (cons (cdr (cadr exp)) (cddr exp))) *global-env*)) *global-env*)
             )))        
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
   ((eq? (car exp) 'cond)
    (handle-cond (cdr exp) env))
   (else
    (handle-call (map (lambda (sub-exp) (my-eval sub-exp env)) exp)))
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
    
(define (handle-cond expressions env)
  
    ;; We first need to check if the first expression starts with an Else, and then if (car (car expressions)) is true
    ;; It it is true, we need to handle the block after that expression
    ;; Else, just go to the next expression with recursive call
  (let ((first_true (if (eq? (car (car expressions)) 'else) #t (my-eval (car (car expressions)) env))))
    (if first_true (handle-block (cdr (car expressions)) env) (handle-cond (cdr expressions) env))
    )
  )

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
    (cond ((pair? body) 
            ;; Create Uninitialized takes care of Step 1 listed above
            ;; Update Uninitialzed List takes care of Steps 2, 3 and 4 from above
            ;; Handle-block takes care of Step 5 from above
           
            (let ((envNew (update-uninitialized-list defs (create-uninitialized-list defs env))))
                (handle-block body envNew))

       )))

;; Creates list of all vars as pair with *undefined 
;; If we have var x and y, list will be pairs that looks like ((x *undefined) (y *undefined))
(define (create-uninitialized-list defs undefinedList)
   (cond ((null? defs) undefinedList)
         (else 
          (create-uninitialized-list (cdr defs) (cons (list (car (car defs)) '*undefined) undefinedList))
         )))

;; Update the Uninitialized List
;; Makes Env now have appropriate operation instead of '*undefined as second part of pair
(define (update-uninitialized-list defs env)
  (cond ((null? defs)
         env)
        ((pair? defs)
             (cond ((pair? (cadr (car defs)))
                     (set-cdr! (assoc (car (car defs)) env) (list (my-eval (cadr (car defs)) env))))
                    (else  
                      (set-cdr! (assoc (car (car defs)) env) (my-eval (cadr (car defs)) env)))
             )
            (update-uninitialized-list (cdr defs) env))
    ))

(define (append-each-list-item newList oldList)
    (cond ((null? newList) oldList)
          (else (append-each-list-item (cdr newList) (cons (car newList) oldList)))
))


(define (handle-let defs body env)
  ;; This is going to create a new environment, and then we handle block with new env
    (let ((newEnv (handle-let-defs-to-env defs env env)))
      (handle-block body newEnv))
)   
    
;; Takes all the def's and returns 
;; This will keep original env, make a new env, but will
;; continute to evaluate each expression with original env
;; Should initially  pass in (Defs, envOrig, envOrig)
(define (handle-let-defs-to-env defs envOrig envNew)
  (cond ((null? defs) envNew)
    ((pair? defs) 
        (handle-let-defs-to-env (cdr defs) envOrig (cons (list (car (car defs)) (my-eval (cadr (car defs)) envOrig)) envNew))
	)))
		
;; Unlike handle-let, let* allows recursion, for each new def can have previous def in.
;; We don't need to store orig and new env, we just keep updating env with each def and use
;; new env to analyze each new def
(define (handle-let* defs body env)
  (cond ((pair? defs) 
         (handle-let* (cdr defs) body (cons (list (car (car defs)) (my-eval (cadr (car defs)) env)) env)))
         ((null? defs)
          (handle-block body env))
          ))

;; Handling calls like a boss
(define (handle-call evald-exps)
  (let ((fn (car evald-exps))
    (args (cdr evald-exps)))
   (cond
    ((eq? (car fn) 'closure) 
     (let ((formals (cadr (cadr fn)))
       (body (cddr (cadr fn)))
       (env (caddr fn)))
       (handle-block body (append (bind formals args) env))))
    ((eq? (car fn) 'primitive-function)
     (apply (cadr fn) args))
    (else (display "Error: Calling non-function"))
    )))

;; We must add this into our Global env as well, associate it so
;; When repl reads newline, we instead call this function instead
;; of the built in newline
  (define (newline2)
    (display "\n"))

;; Handling Applys
(define (my-apply function arguments)
  ;; Func will come in as ('primitive-function built-in-function) 
  ;; Or they will come in as ('closure lambda-exp env)
  (cond ((eq? (car function) 'primitive-function)
         (apply (cadr function) arguments))
        ((eq? (car function) 'closure)
         (let ((function-actions-list (cons ((list function) arguments))))
         (handle-call function-actions-list))) 
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
    (list 'apply (list 'primitive-function my-apply))
    ))
