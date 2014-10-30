(define (test-letrec)
  (letrec
      ((f (lambda (x) (if (= x 0) 1 (* x (g (- x 1))))))
       (g (lambda (x) (if (= x 0) 1 (* x (f (- x 1)))))))
    (display "Should return 120") (newline)
    ;; (f 5)
    (f 1)
    ))

