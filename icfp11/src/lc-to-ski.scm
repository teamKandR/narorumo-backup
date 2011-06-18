;; Translate lambda calculus to SKI.

#lang scheme

(include "lib/pmatch.scm")

;; T(λx.M) = A(x,T(M))
;; T(MN) = T(M) T(N)
;; T(x) = x

;; where

;; A(x,x) = I
;; A(x,y) = K y   (when y is different from x)
;; A(x,MN) = S (A(x,M)) (A(x,N))

(define T
  (lambda (expr)
    (pmatch expr
      [,var (guard (symbol? var)) var]
      [(lambda (,x) ,M) (A x (T M))]
      [(,M ,N) `(,(T M) ,(T N))])))

(define A
  (lambda (x y)
    (pmatch `(,x ,y)
      [(,x ,y) (guard (symbol? y) (equal? x y)) 'I]
      [(,x ,y) (guard (symbol? y)) `(K ,y)]
      [(,x (,M ,N)) `(S ,(A x M) ,(A x N))])))
