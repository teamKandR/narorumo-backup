;; the datas.
(define lindsey-miles-2007-11
  '(2.6 2.6 3.7 4.8 5.2 6.8 1.6 ;;; 11/01/2007-11/07/2007
    1.0 2.6 2.6 3.7 2.8 1.5 2.6 ;;; 11/08/2007-11/14/2007
    2.6 3.7 3.7 2.6 3.7 2.6 3.7 ;;; 11/15/2007-11/21/2007
    3.7 3.7 3.7 3.7 3.7 3.7 3.7 ;;; 11/22/2007-11/28/2007
    3.7 3.7))                   ;;; 11/29/2007-11/30/2007

(define alex-miles-2007-11
  '(5.0 7.1 5.0 3.6 7.2 4.3 4.0 ;;; 11/01/2007-11/07/2007
   13.9 3.6 3.2 1.2 8.8 4.4 3.6 ;;; 11/08/2007-11/14/2007
    7.8 3.6 6.5 8.8 3.6 3.6 4.4 ;;; 11/15/2007-11/21/2007
   12.3 3.2 6.5 4.4 5.0 6.5 6.2 ;;; 11/22/2007-11/28/2007
    4.4 3.7))                   ;;; 11/29/2007-11/30/2007

(define lindsey-miles-2008-05
  '(3.7 ;;; 1 Thursday http://www.gmap-pedometer.com/?r=1430525
    3.7 ;;; 2 Friday                    "
    3.7 ;;; 3 Saturday                  "
    3.7 ;;; 4 Sunday                    "
    3.6 ;;; 5 Monday http://www.gmap-pedometer.com/?r=1869316
    3.7 ;;; 6 Tuesday http://www.gmap-pedometer.com/?r=1430525
    3.7 ;;; 7 Wednesday                 "
    2.6 ;;; 8 Thursday http://www.gmap-pedometer.com/?r=1426927
    2.1 ;;; 9 Friday http://www.gmap-pedometer.com/?r=1884791
    5.8 ;;; 10 Saturday http://www.gmap-pedometer.com/?r=1884921    
    1.3 ;;; 11 Sunday http://www.gmap-pedometer.com/?r=1886211
    2.0 ;;; 12 Monday http://www.gmap-pedometer.com/?r=1899098
    2.6 ;;; 13 Tuesday http://www.gmap-pedometer.com/?r=1426927
    2.6 ;;; 14 Wednesday                "
    2.6 ;;; 15 Thursday                 "
    2.6 ;;; 16 Friday                   "
    2.6 ;;; 17 Saturday                 "
    2.6 ;;; 18 Sunday                   "
    2.6 ;;; 19 Monday                   "
    2.6 ;;; 20 Tuesday                  "
    3.6 ;;; 21 Wednesday http://www.gmap-pedometer.com/?r=1869316
    3.6 ;;; 22 Thursday                 "
    6.3 ;;; 23 Friday http://www.gmap-pedometer.com/?r=818250
    2.6 ;;; 24 Saturday http://www.gmap-pedometer.com/?r=1426927
    3.6 ;;; 25 Sunday http://www.gmap-pedometer.com/?r=1869316
    ))

(define alex-miles-2008-05
  '(3.6 ;;; 1 Thursday http://www.gmap-pedometer.com/?r=1432587
    4.3 ;;; 2 Friday http://www.gmap-pedometer.com/?r=1436748
    6.1 ;;; 3 Saturday http://www.gmap-pedometer.com/?r=1872789 
    3.6 ;;; 4 Sunday http://www.gmap-pedometer.com/?r=1872765
    5.2 ;;; 5 Monday http://www.gmap-pedometer.com/?r=1872774
    4.3 ;;; 6 Tuesday http://www.gmap-pedometer.com/?r=1436748
    3.6 ;;; 7 Wednesday http://www.gmap-pedometer.com/?r=1872765 barefoot shoes
    7.1 ;;; 8 Thursday http://www.gmap-pedometer.com/?r=1476515, plus 1 mile on
        ;;;   treadmill (fast) to test DEFYANCE shoes.
    2.1 ;;; 9 Friday http://www.gmap-pedometer.com/?r=1884791
    5.8 ;;; 10 Saturday http://www.gmap-pedometer.com/?r=1884921    
    1.3 ;;; 11 Sunday http://www.gmap-pedometer.com/?r=1886211
    4.3 ;;; 12 Monday http://www.gmap-pedometer.com/?r=1891491
    5.5 ;;; 13 Tuesday http://www.gmap-pedometer.com/?r=1895305 barefoot shoes
    6.1 ;;; 14 Wednesday http://www.gmap-pedometer.com/?r=1899204
    4.3 ;;; 15 Thursday http://www.gmap-pedometer.com/?r=1436748
    3.6 ;;; 16 Friday http://www.gmap-pedometer.com/?r=1432587
   11.4 ;;; 17 Saturday http://www.gmap-pedometer.com/?r=1907860
    5.3 ;;; 18 Sunday http://www.gmap-pedometer.com/?r=1908748
    1.6 ;;; 19 Monday http://www.gmap-pedometer.com/?r=1910846
    4.3 ;;; 20 Tuesday http://www.gmap-pedometer.com/?r=1436748
    6.5 ;;; 21 Wednesday http://www.gmap-pedometer.com/?r=1918365
    5.2 ;;; 22 Thursday http://www.gmap-pedometer.com/?r=1921340
    7.3 ;;; 23 Friday http://www.gmap-pedometer.com/?r=1923914
    6.8 ;;; 24 Saturday http://www.gmap-pedometer.com/?r=1926451 
    4.3 ;;; 25 Sunday http://www.gmap-pedometer.com/?r=1436748
    3.5 ;;; 26 Monday http://www.gmap-pedometer.com/?r=1932373
    ))                 

;; Sadly, inexact->exact didn't quite do what I wanted!
(define (exactify lst)
    (let ((exactify-item
          (lambda (num)
            (string->number (string-append "#e" (number->string num))))))
      (map exactify-item lst)))

;; first few stats functions -- what other numbers might we want to calculate?
(define (total lst)
  (apply + lst))

(define (average lst)
  (/ (apply + lst) (length lst)))

;; returns a nested list; the inside ones are of the form (miles count),
;; where count is the number of runs done at that distance
(define (histogram lst)
  (letrec ((sorted (quicksort lst))
           (histogram-help
            (lambda (prev-val prev-count lst)
              (cond
                ((null? lst) (if prev-val
                                 (list (list prev-val prev-count))
                                 null))
                ((equal? prev-val (car lst))
                 (histogram-help prev-val (+ 1 prev-count) (cdr lst)))
                (#t
                 (let ((rst (histogram-help (car lst) 1 (cdr lst))))
                   (if prev-val
                       (cons (list prev-val prev-count) rst)
                       rst)))))))    
    (histogram-help #f #f sorted)))

;; utility functions -- so we don't need to include any PLT libraries.
(define (filter pred lst)
  (cond
    ((null? lst) '())
    ((pred (car lst))
     (cons (car lst) (filter pred (cdr lst))))
    (#t (filter pred (cdr lst)))))

(define (quicksort lst)
  (cond
    ((null? lst) '() )
    (#t
     (let* ((pivot (car lst))
            (firstpart (quicksort
                        (filter (lambda(x) (< x pivot)) (cdr lst))))
            (lastpart (quicksort
                       (filter (lambda(x) (>= x pivot)) (cdr lst)))))
       (append firstpart (list pivot) lastpart)))))

;; Test cases
;(total (exactify lindsey-miles-2007-11))


