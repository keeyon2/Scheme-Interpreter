(define one 100)

(define two 200)

(define (test-let)
  (let ((one 1) (two (+ one 1)))
    (display "Should return 101") (newline)
    two))

(define (test-let*)
  (let* ((one 1) (two (+ one 1)))
    (display "Should return 2") (newline)
    two))


(define (test-lambda)
  (display ((lambda (x) (+ x x)) 4)))

(define (foo x y) (+ x y))

(define (test-define-func)
  (display "Should return 300") (newline)
  (foo one two))

(define (test-letrec)
  (letrec
      ((f (lambda (x) (if (= x 0) 1 (* x (g (- x 1))))))
       (g (lambda (x) (if (= x 0) 1 (* x (f (- x 1)))))))
    (display "Should return 120") (newline)
    (f 5)
    ))

(define (test-cond)
   (display "Should return 100") (newline)
   (cond ((= one two) 3)
     ((= one (* one 1))
          (cond ((null? '(1 2 3)) 4)
        (else (- two one))
        ))
     (else 5)))

(define (add5 x) (+ 5 x))

(define (test-apply)
  (display "Should return 20") (newline)
  (apply add5 (list 15)))
