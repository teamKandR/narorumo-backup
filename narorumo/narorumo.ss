;; the datas.
(define lindsey-miles
  '(2.6 2.6 3.7 4.8 5.2 6.8 1.6 ;;; 11/01/2007-11/07/2007
    1.0 2.6 2.6 3.7 2.8 1.5 2.6 ;;; 11/08/2007-11/14/2007
    2.6 3.7 3.7 2.6 3.7 2.6 3.7 ;;; 11/15/2007-11/21/2007
    3.7 3.7 3.7 3.7 3.7 3.7 3.7 ;;; 11/22/2007-11/28/2007
    3.7 3.7))                   ;;; 11/29/2007-11/30/2007

(define alex-miles
  '(5.0 7.1 5.0 3.6 7.2 4.3 4.0 13.9 3.6 3.2 1.2 8.8 4.4 3.6 7.8 3.6 6.5 8.8 3.6
        3.6 4.4 12.3 3.2 6.5 4.4 5.0 6.5 6.2 4.4 3.7))

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
(total (exactify lindsey-miles))


