(define one 100)


(define two 200)

;;  (define (test-let)
;;  	(let ((y 7) (k 10)) (+ 7 1))
;;      )

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
