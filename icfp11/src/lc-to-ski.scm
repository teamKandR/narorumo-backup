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
      [,var (guard (or (number? var) (symbol? var))) var]
      [(lambda (,x) ,M) (A x (T M))]
      [(,M ,N) `(,(T M) ,(T N))])))

(define A 
  (lambda (x y)
    (pmatch `(,x ,y) 
      ;; x is the formal parameter of a lambda,
      ;; so it's definitely a variable
      [(,x ,y) (guard (symbol? y) (equal? x y)) 'I]
      [(,x ,y) (guard (or (number? y) (symbol? y))) `(K ,y)]
      [(,x (,M ,N)) `((S ,(A x M)) ,(A x N))])))

;; Optimization pass.
(define optimize-ski
  (lambda (ski)
    (pmatch ski
      [,var (guard (symbol? var)) var]
      [((S (K K)) I) 'K]
      [(,M ,N) `(,(optimize-ski M) ,(optimize-ski N))])))

;; Our mnemonics for various common SKI expressions.

(define names
  (lambda (e)
    (pmatch e
      [,var (guard (symbol? var)) var]
      [(S (K (S I I))) 'abe]
      [(S (S (K S) K) (K (S I I))) 'benny]
      [(S (K S) K) 'clarice]
      [(K (S I I)) 'dianne]
      [((S (K K)) (K K)) 'fanny]
      [(S (K K)) 'greg]
      [(S (K S)) 'horace]
      [(K I) 'ian]
      [(,M ,N) `(,(names M) ,(names N))])))

(define unnames
  (lambda (e)
    (pmatch e
      [,var (guard (symbol? var)) var]
      [abe '(S (K (S I I)))]
      [benny '(S (S (K S) K) (K (S I I)))]
      [clarice '(S (K S) K)]
      [dianne '(K (S I I))]
      [fanny '((S (K K)) (K K))]
      [greg '(S (K K))]
      [horace '(S (K S))]
      [ian '(K I)]
      [(,M ,N) `(,(unnames M) ,(unnames N))])))

(define metanames
  (lambda (e)
    (pmatch e
      [,var (guard (symbol? var)) var]
      [(S (horace (greg (K S)))) 'june1]
      [((S (horace fanny)) (greg I)) 'june2]
      [((S (horace fanny)) ian) 'kelly]
      [(,M ,N) `(,(metanames M) ,(metanames N))])))

(define unmetanames
  (lambda (e)
    (pmatch e
      [,var (guard (symbol? var)) var]
      [june1 '(S (horace (greg (K S))))]
      [june2 '((S (horace fanny)) (greg I))]
      [kelly '((S (horace fanny)) ian)]
      [(,M ,N) `(,(unmetanames M) ,(unmetanames N))])))

(define metametanames
  (lambda (e)
    (pmatch e
      [,var (guard (symbol? var)) var]
      [(S (horace (june1 june2))) 'june]
      [(,M ,N) `(,(metametanames M) ,(metametanames N))])))

(define unmetametanames
  (lambda (e)
    (pmatch e
      [,var (guard (symbol? var)) var]
      [june '(S (horace (june1 june2)))]
      [(,M ,N) `(,(unmetametanames M) ,(unmetametanames N))])))

(define allthenames
  (lambda (ski)
    (metametanames (metanames (names ski)))))

(define unallthenames
  (lambda (metametanamed)
    (unnames (unmetanames (unmetametanames (metametanamed))))))

(define translate
  (lambda (exp)
    (allthenames (T exp))))

(define translate-optimized
  (lambda (exp)
    (allthenames (optimize-ski (T exp)))))



