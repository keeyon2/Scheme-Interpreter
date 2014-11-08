;;2nd element
(define (cadr L)
    (car (cdr L)))

;; All but first 2
(define (cddr L)
    (cdr (cdr L)))

;; All but first 3
(define (cdddr L)
    (cdr (cdr (cdr L))))

;; Third element
(define (caddr L)
    (car (cdr (cdr L))))

;; Forth Element
(define (cadddr L)
    (car (cdr (cdr (cdr L)))))

;; Newline
(define (newline)
  (display "\n"))

;; Append
(define (append L1 L2)
  (cond ((null? L1) L2)
	(else (cons (car L1) (append (cdr L1) L2)))
	))

;; Map
(define (map func L)
  (cond ((null? L) '())
	(else (cons (func (car L)) (map func (cdr L))))
	))

;; Test Func - Remove me
(define (testFunc x)
  (+ x 2))

;; And
(define (and x y)
  (if x y #f))

;; Or
(define (or x y)
  (if x #t y))

;; Not
(define (not x)
  (if x #f #t))

(define (equal? Item1 Item2)
  (cond
    ;; Item 1 is Pair
    ((pair? Item1)
     (cond 
       ;; Item 1 and 2 are Pair, check for recurs
       ((pair? Item2)
        (if (equal? (car Item1) (car Item2)) (equal? (cdr Item1) (cdr Item2)) #f))
       ;; Item 1 is Pair, Item 2 is not
       (else
         #f)))
    ;; Item 1 is not a pair
    (else 
      (cond
        ((pair? Item2)
         #f)
        (else
          (eq? Item1 Item2)
          )))
    ))

;; Equal Orig?
(define (equal?Orig L1 L2)
  (cond 
	;; Base Cases 
	((null? L1)
	  (if (null? L2) #t #f))

	;; Same base case as above but opposite list
	;; Redundant code but easier to understand
	((null? L2)
	  (if (null? L1) #t #f))

	;; L1 not list
	((not (pair? L1))
	 (cond ((not (pair? L2)) (= L1 L2))
	       ((pair? L2) #f)
	       ))

	;; L1 is a list
	((pair? L1)
	 (cond ((not (pair? L2)) #f)
		((pair? L2)
		  	(cond 
                 		((eq? (car L1) (car L2)) (equal? (cdr L1) (cdr L2)))
		 		(else #f))
			)))))

;; Assoc
(define (assoc obj L1)
  (cond ((not (pair? L1)) #f)
	(else 
	  (if (equal? obj (car (car L1))) (car L1) (assoc obj (cdr L1))))
	))

