(define (equalKee Item1 Item2)
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

(define (test-equal)
  (display "Testing many cases, we should display CORRECT many times")
  (newline)
  ;; Null and Not Null
  (if (equalKee '() 4) (display "1. Incorrect") (display "1. Correct"))
  (newline)
  
  ;; Null and Null
  (if (equalKee '() '()) (display "2. Correct ") (display "2. Incorrect"))
  (newline)

  ;; Not Null and Null
  (if (equalKee 4 '()) (display "3. Incorrect") (display "3. Correct"))
  (newline)

  ;; Null and Null
  (if (equalKee '() '()) (display "4. Correct ") (display "4. Incorrect"))
  (newline)

  ;; L1 Not a list.  Equal
  (if (equalKee 4 4) (display "5. Correct") (display "5. Incorrect"))
  (newline)

  ;; L1 Not a list, not equal
  (if (equalKee 4 5) (display "6. Incorrect") (display "6. Correct")) 
  (newline)

  ;; L1 Not a list, other is a list
  (if (equalKee 4 '(4 4)) (display "7. Incorrect") (display "7. Correct"))
  (newline)

  ;; L1 a list.  Equal
  (if (equalKee '(4) '(4)) (display "8. Correct") (display "8. Incorrect"))
  (newline)

  ;; L1 a list.  Equal
  (if (equalKee '(4 5 6) '(4 5 6)) (display "9. Correct") (display "9. Incorrect"))
  (newline)

  ;; L1 a list, not equal
  (if (equalKee '(4 5 6) '(4 5 7)) (display "10. Incorrect") (display "10. Correct")) 
  (newline)
  
  (if (equalKee '(((1 2) (3 4) 5)) '(((1 2) (3 4) 5))) (display "11. Correct") (display "11. Incorrect"))
    (newline)
  )
