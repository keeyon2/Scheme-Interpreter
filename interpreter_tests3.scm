(define (add5 x) (+ 5 x))

(define (test-apply)
  (display "Should return 20") (newline)
  (apply add5 (list 15)))
