;; generate-tautologies: A little miniKanren program to generate
;; tautologies represented as S-expressions, inspired by Peter
;; Boothe's version at
;; http://imprompt.us/2010/generate-a-random-true-boolean-lisp-statement

;; miniKanren and the "matche" pattern matcher are freely available at
;; http://iucs-relational-research.googlecode.com

;; Usage:
;; > (generate-tautologies 10)
;; (1 (not 0) (and 1 1) (or 1 _.0) (or _.0 1) (not (and 0 _.0))
;;    (not (not 1)) (not (and _.0 0)) (and 1 (not 0))
;;    (and (not 0) 1))

(import (minikanren vanilla))
(load "matche.scm")

(define true-expro
  (lambda (expr)
    (matche expr
      [1]
      [(and ,expr1 ,expr2)
       (true-expro expr1)
       (true-expro expr2)]
      [(or ,expr1 ,expr2)
       (conde
         [(true-expro expr1)]
         [(true-expro expr2)])]
      [(not ,expr1)
       (false-expro expr1)])))

(define false-expro
  (lambda (expr)
    (matche expr
      [0]
      [(and ,expr1 ,expr2)
       (conde
         [(false-expro expr1)]
         [(false-expro expr2)])]
      [(or ,expr1 ,expr2)
       (false-expro expr1)
       (false-expro expr2)]
      [(not ,expr1)
       (true-expro expr1)])))

(define generate-tautologies
  (lambda (n)
    (run n (q) (true-expro q))))
