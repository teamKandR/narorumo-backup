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

;; Could've also been done with reduce:

(define reduce
  (lambda (f ls)
    (letrec ((reduce
              (lambda (f ls acc)
                (cond
                  [(null? ls) acc]
                  [(null? (cdr ls))  (f (car ls) acc)]
                  [else (reduce f (cddr ls) (+ acc (f (car ls) (cadr ls))))]))))
      (reduce f ls 0))))

(define problem-1-with-reduce
  (lambda (n)
    (display
     (reduce + (filter (lambda (x)
                         (or (= (remainder x 5) 0)
                             (= (remainder x 3) 0)))
                       (letrec ((range (lambda (n)
                                         (cond
                                           [(zero? n) '()]
                                           [else (cons n (range (- n 1)))]))))
                         (range (- n 1))))))))

(problem-1-with-reduce 1000)

;; I realized yesterday (took me long enough!) that in Scheme we
;; almost have sum already, since the + primitive is variable-arity.
;; So we don't really need reduce (or fold-left).  And if we define
;; range (which is easy) and make a quick alias for %, we suddenly
;; have something that's just about as concise as the equivalent
;; Python, and arguably more readable because we don't have to think
;; about operator precedence!

(define sum (lambda (ls) (apply + ls)))
(define % remainder)
(define range (lambda (n)
                (cond
                  [(zero? n) '()]
                  [else (cons (- n 1) (range (- n 1)))])))

(display (sum (filter (lambda (x)
                        (or (= (% x 5) 0)
                            (= (% x 3) 0)))
                      (range 1000))))

;; There -- now I feel better.
