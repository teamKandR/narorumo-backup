#!r6rs
;; generate-tautologies: A little miniKanren program to generate
;; tautologies represented as S-expressions, inspired by Peter
;; Boothe's version at
;; http://imprompt.us/2010/generate-a-random-true-boolean-lisp-statement

;; The code requires that the miniKanren 'vanilla' library be
;; installed.  It's available here:
;; https://github.com/iucs/relational-research/blob/master/lib/minikanren/vanilla.sls

;; The 'matche' library also has to be installed.  It's in the same
;; directory as this file.

;; Usage:
;; > (generate-tautologies 10)
;; (1 (not 0) (and 1 1) (or 1 _.0) (or _.0 1) (not (and 0 _.0))
;;    (not (not 1)) (not (and _.0 0)) (and 1 (not 0))
;;    (and (not 0) 1))

;; Discussion here: http://lindseykuper.livejournal.com/345176.html

(library (tautologies)
         (export generate-tautologies generate-tautologies-concretized) 
         (import (rnrs) (minikanren vanilla) (matche))

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

         ;; We can also generate tautologies explicitly, with no
         ;; unconcretized variables, by explicitly writing out each
         ;; way in which (or expr1 expr2) can be true and each way in
         ;; which (and expr1 expr2) can be false.

         ;; > (generate-tautologies-concretized 10)
         ;; (1 (and 1 1) (or 1 0) (not 0) (or 0 1) (and 1 (and 1 1))
         ;;    (and (and 1 1) 1) (or 1 1) (and 1 (or 1 0))
         ;;    (or 1 (and 0 0)))

         (define true-expro-concretized
           (lambda (expr)
             (matche expr
                     [1]
                     [(and ,expr1 ,expr2)
                      (true-expro-concretized expr1)
                      (true-expro-concretized expr2)]
                     [(or ,expr1 ,expr2)
                      (true-expro-concretized expr1)
                      (false-expro-concretized expr2)]
                     [(or ,expr1 ,expr2)
                      (false-expro-concretized expr1)
                      (true-expro-concretized expr2)]
                     [(or ,expr1 ,expr2)
                      (true-expro-concretized expr1)
                      (true-expro-concretized expr2)]
                     [(not ,expr1)
                      (false-expro-concretized expr1)])))

         (define false-expro-concretized
           (lambda (expr)
             (matche expr
                     [0]
                     [(and ,expr1 ,expr2)
                      (false-expro-concretized expr1)
                      (false-expro-concretized expr2)]
                     [(and ,expr1 ,expr2)
                      (true-expro-concretized expr1)
                      (false-expro-concretized expr2)]
                     [(and ,expr1 ,expr2)
                      (false-expro-concretized expr1)
                      (true-expro-concretized expr2)]
                     [(or ,expr1 ,expr2)
                      (false-expro-concretized expr1)
                      (false-expro-concretized expr2)]
                     [(not ,expr1)
                      (true-expro-concretized expr1)])))

         (define generate-tautologies-concretized
           (lambda (n)
             (run n (q) (true-expro-concretized q))))

         )


