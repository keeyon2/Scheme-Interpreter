(define (PPrint)
  (display "FINE I GET IT"))

(define (newline2)
  (display "\n"))

(define (append2 L1 L2)
  (cond ((null? L1) L2)
	(else (cons (car L1) (append2 (cdr L1) L2)))
	))

(define (map2 func L)
  (cond ((null? L) '())
	(else (cons (func (car L)) (map2 func (cdr L))))
	))

(define (testFunc x)
  (+ x 2))

(define testList '(1 2 3 4 5 6 7))

(define (and2 x y)
  (if x y #f))

(define (or2 x y)
  (if x #t y))

(define (not2 x)
  (if x #f #t))

(define (equal2? x y)
  (if (if x y #f) #t (if (not2 x) (not2 y) #f)))
  
(define (equal3? L1 L2)
  (cond 
    	;; If NPair - Pair	

	;; Base Cases 
	((null? L1)
	  (if (null? L2) #t #f))

	;; Same base case as above but opposite list
	;; Redundant code but easier to understand
	((null? L2)
	  (if (null? L1) #t #f))

	;; L1 not list
	((not2 (pair? L1))
	 (cond ((not2 (pair? L2)) (= L1 L2))
	       ((pair? L2) #f)
	       ))

	;; L1 is a list
	((pair? L1)
	 (cond ((not2 (pair? L2)) #f)
		((pair? L2)
		  	(cond 
                 		((eq? (car L1) (car L2)) (equal3? (cdr L1) (cdr L2)))
		 		(else #f))
			)))))

(define (assoc2 obj L1)
  (cond ((not2 (pair? L1)) #f)
	(else 
	  (if (equal3? obj (car (car L1))) (car L1) (assoc2 obj (cdr L1))))
	))

(define L '((1 2) ( 3 4) ( 5 6) ( 7 8) (31 9)))

