;; the datas.
(define lindsey-miles
  '(2.7 2.7 3.7 4.8 5.2 6.8 1.6 1.0 2.7 2.7 3.7 2.9 1.5 2.7 2.7 3.7 3.7 2.7 3.7
        3.7 3.7 3.7 3.7 3.7 3.7 3.7 3.7 3.7 3.7 3.7))

(define alex-miles
  '(5.0 7.1 5.0 3.6 7.2 4.3 4.0 13.9 3.6 3.2 1.2 8.8 4.4 3.6 7.8 3.6 6.5 8.8 3.6
        3.6 4.4 12.3 3.2 6.5 4.4 5.0 6.5 6.2 4.4 3.7))

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