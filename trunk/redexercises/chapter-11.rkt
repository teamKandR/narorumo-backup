#lang racket
(require redex)

;; Exercises from Part II of the PLT Redex book.

;; Chapter 11: The Basics

;; 11.1

;; What does this produce?
(term (+ ,(first (term (,(+ 12 34)))) 5))

;; Well, the quoted bits get evaluated, so (+ 12 34) evals to 46, and
;; then we've got (term (46)), which gets turned into (list 46).  Then
;; (first (list 46)) gets eval'd, giving us 46.  Then we have (term (+
;; 46 5)), producing (list '+ 46 5), I think.

;; ...Yep!  The REPL gives us '(+ 46 5).

;; 11.2

;; Grammar of simple addition expressions

(define-language A
  [Num 0
       1
       2
       (+ Num Num)]
  [Ctx (+ Ctx Num)
       (+ Num Ctx)
       hole])

;; Seems to work okay:

;; racket@redex-book.rkt> (redex-match A Num (term 0))
;; (list (match (list (bind 'Num 0))))
;; racket@redex-book.rkt> (redex-match A Num (term 3))
;; #f
;; racket@redex-book.rkt> (redex-match A (in-hole Ctx 2) (term (+ 2 (+ 2 1))))
;; (list
;;  (match (list (bind 'Ctx (list '+ (hole) '(+ 2 1)))))
;;  (match (list (bind 'Ctx (list '+ 2 (list '+ (hole) 1))))))


;; 11.3

;; A reduction system for mod 3 arithmetic!

(define A-red-mod3
  (reduction-relation
   A
   ;; 0 + n or n + 0 = n
   (--> (in-hole Ctx (+ 0 Num))
        (in-hole Ctx Num)
        plus-zero-left)
   (--> (in-hole Ctx (+ Num 0))
        (in-hole Ctx Num)
        plus-zero-right)
   ;; 1 + n or n + 1 = { 1+n if n = 0 or 1; 0 if n = 2}
   ;; The n = 0 cases are already covered
   (--> (in-hole Ctx (+ 1 1))
        (in-hole Ctx 2)
        plus-one-one)
   (--> (in-hole Ctx (+ 1 2))
        (in-hole Ctx 0)
        plus-one-two)
   (--> (in-hole Ctx (+ 2 1))
        (in-hole Ctx 0)
        plus-two-one)
   ;; 2 + n or n + 2 = { 2+n if n = 0; 0 if n = 1; 1 if n = 2}
   ;; The n = 0 and n = 1 cases are already covered
   (--> (in-hole Ctx (+ 2 2))
        (in-hole Ctx 1)
        plus-two-two)
   ))

;; 11.4

;; Here are a few examples -- nice and diamondy!

(define (traces-A-example-0)
  (traces A-red-mod3 (term (+ 0 (+ 2 2)))))

(define (traces-A-example-1)
  (traces A-red-mod3 (term (+ (+ (+ (+ (+ 1 0) 1) 0) 1) 0))))

(define (traces-A-example-2)
  (traces A-red-mod3 (term (+ 1 (+ 0 (+ 1 (+ 0 (+ 1 0))))))))

;; 11.5

;; Try to make our language do leftmost-outermost ("standard")
;; reductions only.  The operative word being "try", I guess.

(define-language A-std
  [Num 0
       1
       2
       (+ Num Num)]
  [Ctx (+ Ctx Num)
       hole])

(define A-std-red-mod3
  (reduction-relation
   A-std
   (--> (in-hole Ctx (+ 0 Num))
        (in-hole Ctx Num)
        plus-zero-left)
   (--> (in-hole Ctx (+ 1 1))
        (in-hole Ctx 2)
        plus-one-one)
   (--> (in-hole Ctx (+ 1 2))
        (in-hole Ctx 0)
        plus-one-two)
   (--> (in-hole Ctx (+ 2 1))
        (in-hole Ctx 0)
        plus-two-one)
   (--> (in-hole Ctx (+ 2 2))
        (in-hole Ctx 1)
        plus-two-two)
   ))

;; This one works...
(define (traces-A-std-example-0)
  (traces A-std-red-mod3 (term (+ 0 (+ 2 2)))))

;; ...but these two don't reduce!
(define (traces-A-std-example-1)
  (traces A-std-red-mod3 (term (+ (+ (+ (+ (+ 1 0) 1) 0) 1) 0))))

(define (traces-A-std-example-2)
  (traces A-std-red-mod3 (term (+ 1 (+ 0 (+ 1 (+ 0 (+ 1 0))))))))
