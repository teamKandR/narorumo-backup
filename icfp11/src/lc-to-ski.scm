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

(define names
  (lambda (e)
    (pmatch e
      [,var (guard (symbol? var)) var]
      [((S (K K)) (K K)) 'fanny]
      [(S (K K)) 'greg]
      [(S (K S)) 'horace]
      [(K I) 'ian]
      [(,M ,N) `(,(names M) ,(names N))])))

#|

Eventually, we want to write:

(Y (lambda (f)
     (lambda (slot)
       ((lambda (ignoreme)
          (f slot))
        (dec slot)))))

The argument to Y,

(lambda (f)
  (lambda (slot)
    ((lambda (ignoreme)
       (f slot))
     (dec slot))))

, translates to

((S
  ((S (K S))
   ((S
     ((S (K S))
      ((S ((S (K S)) ((S (K K)) (K S)))) ((S ((S (K S)) ((S (K K)) (K K)))) ((S (K K)) I)))))
    ((S ((S (K S)) ((S (K K)) (K K)))) (K I)))))
 ((S ((S (K S)) ((S (K K)) (K dec)))) (K I)))

, or we might also like to do the moral equivalent of

(begin
  (dec slot)
  (dec slot)
  (f slot))

, so the whole thing would be

(lambda (f)
  (lambda (slot)
    ((lambda (ignoremetoo)
       ((lambda (ignoreme)
          (f slot))
        (dec slot)))
     (dec slot))))

which, translated, looks like:

((S
  ((S (K S))
   ((S
     ((S (K S))
      ((S ((S (K S)) ((S (K K)) (K S))))
       ((S
         ((S (K S))
          ((S ((S (K S)) ((S (K K)) (K S))))
           ((S
             ((S (K S))
              ((S ((S (K S)) ((S (K K)) (K S))))
               ((S ((S (K S)) ((S (K K)) (K K)))) ((S (K K)) (K S))))))
            ((S
              ((S (K S))
               ((S ((S (K S)) ((S (K K)) (K S))))
                ((S ((S (K S)) ((S (K K)) (K K)))) ((S (K K)) (K K))))))
             ((S ((S (K S)) ((S (K K)) (K K)))) ((S (K K)) I)))))))
        ((S
          ((S (K S))
           ((S ((S (K S)) ((S (K K)) (K S))))
            ((S ((S (K S)) ((S (K K)) (K K)))) ((S (K K)) (K K))))))
         ((S ((S (K S)) ((S (K K)) (K K)))) (K I)))))))
    ((S
      ((S (K S))
       ((S ((S (K S)) ((S (K K)) (K S)))) ((S ((S (K S)) ((S (K K)) (K K)))) ((S (K K)) (K dec))))))
     ((S ((S (K S)) ((S (K K)) (K K)))) (K I))))))
 ((S ((S (K S)) ((S (K K)) (K dec)))) (K I)))

|#



