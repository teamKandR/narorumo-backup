;; Structure and Interpretation of Computer Programs Exercises
;; Chapter 1
;; Lindsey Kuper and Alex Rudnick

;; 1.1 (alexr)
;; What is the result printed by the interpreter in response to each
;; expression? ...


;; 1.2 (alexr)
;; Translate the following expression into prefix form.

(define (one-point-two)
  (/ (+ 5 4 (- 2 (- 3 (+ 6 4/5))))
     (* 3 (- 6 2) (- 2 7))))

;; 1.3 (alexr)
;; Define a procedure that takes three numbers as arguments and returns
;; the sum of squares of the two larger numbers.

(define (square x) (* x x))

(define (sum-of-squares a b)
  (+ (square a) (square b)))

(define (sum-of-squares-of-largest-two a b c)
  (cond
   ;; a is the smallest.
   ((and (< a b) (a c))
    (print 12)
    (sum-of-squares b c))

   ;; b is the smallest.
   ((and (< b a) (< b c))
    (sum-of-squares a c))

   ;; otherwise c is the smallest.
   (#t
    (sum-of-squares a b))))