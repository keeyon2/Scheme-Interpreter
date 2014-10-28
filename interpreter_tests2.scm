(define one 100)

(define two 200)

(define (test-let)
  (let ((one 1) (two (+ one 1)))
    (display "Should return 101") 
    (newline)
    (display two))
  )

(define (test-let*)
  (let* ((one 1) (two (+ one 1)))
    (display "Should return 2") 
    (newline)
    (display two))
  )

(define (test-lambda)
  (display ((lambda (x) (+ x x)) 4)))

(define (foo x y) (+ x y))

(define (test-define-func)
  (display "Should return 300") (newline)
  (foo one two))
