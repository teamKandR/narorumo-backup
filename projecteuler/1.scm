;; Find the sum of all the multiples of 3 or 5 below 1000.

(define problem-1
  (lambda (n)
    (letrec ((kernel (lambda (x acc)
                       (cond [(= x 0) (display acc) (newline)]
                             [(or (= (remainder x 3) 0)
                                  (= (remainder x 5) 0))
                              (kernel (- x 1) (+ acc x))]
                             [else (kernel (- x 1) acc)]))))
      (kernel (- n 1) 0))))

(problem-1 1000)
