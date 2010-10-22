;; Forwards and backwards list-zipping.

;; Discussion here: http://lindseykuper.livejournal.com/359575.html

(import (minikanren vanilla))
(load "matche.scm")

(define zipo
  (lambda (pairoflists listofpairs)
    (matche `(,pairoflists ,listofpairs)
      [( (() ()) () )]
      [( ((,X . ,Xs) (,Y . ,Ys)) ((,X ,Y) . ,XYs) )
       (zipo `(,Xs ,Ys) XYs)])))

;; > (run* (q) (zipo '((1 3 5) (2 4 6)) q))
;; (((1 2) (3 4) (5 6)))
;; > (run* (q) (zipo q '((1 2) (3 4) (5 6))))
;; (((1 3 5) (2 4 6)))
