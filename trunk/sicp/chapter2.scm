;; Structure and Interpretation of Computer Programs Exercises
;; Chapter 2
;; Lindsey Kuper and Alex Rudnick

;; Did we have to define nil somewhere else? PLT doesn't seem to have
;; it by default. -- Alex

;; We just used '() elsewhere.  Silly Alex; nil is for Lisp! --
;; Lindsey
(define nil '())

;;;; 2.1
;; Define a better version of /make-rat/ that handles both positive
;; and negative integers.  /Make-rat/ should normalize the sign so
;; that if the rational number is positive, both the numerator and
;; denominator are positive, and if the rational number is negative,
;; only the numerator is negative.

;; Here are the supporting functions from sicp-booksource.

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define (numer x) (car x))
(define (denom x) (cdr x))

(define (print-rat x)
  (display (numer x))
  (display "/")
  (display (denom x))
  (newline))

;; And here's the exercise itself.

(define make-rat
  (lambda (n d)
    (let ((g (gcd n d)))
      (if (or (and (< n 0) (< d 0))  ; both negative
              (and (> n 0) (> d 0))) ; both positive
          (cons (/ (abs n) g) (/ (abs d) g))
          (cons (/ (- (abs n)) g) (/ (abs d) g)))))) ; neg/pos

;;;; 2.2
;; Consider the problem of representing line segments in a plane.
;; Each segment is represented as a pair of points: a starting point
;; and an ending point.  Define a constructor /make-segment/ and
;; selectors /start-segment/ and /end-segment/ that define the
;; representation of segments in terms of points.  Furthermore, a
;; point can be represented as a pair of numbers: the x coordinate and
;; the y coordinate.  Accordingly, specify a constructor /make-point/
;; and selectors /x-point/ and /y-point/ that define this
;; representation.  Finally, using your selectors and constructors,
;; define a procedure /midpoint-segment/ that takes a line segment as
;; argument and returns its midpoint (the point whose coordinates are
;; the average of the coordinates of the endpoints).

(define (print-point p)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")")
  (newline))

(define make-segment
  (lambda (spt ept)
    (cons spt ept)))

(define start-segment
  (lambda (seg)
    (car seg)))

(define end-segment
  (lambda (seg)
    (cdr seg)))

(define make-point
  (lambda (x y)
    (cons x y)))

(define x-point (lambda (pt) (car pt)))
(define y-point (lambda (pt) (cdr pt)))

(define midpoint-segment
  (lambda (seg)
    (make-point (/ (+ (x-point (start-segment seg))
                      (x-point (end-segment seg)))
                   2)
                (/ (+ (y-point (start-segment seg))
                      (y-point (end-segment seg)))
                   2))))

;; For testing:
(define origin (make-point 0 0))
(define fourthree (make-point 4 3))
(define eightthree (make-point 8 3))
(define negtennegten (make-point -10 -10))
(define littlehorizseg (make-point fourthree eightthree))
(define littlediagseg (make-segment origin negtennegten))
(define bigdiagseg (make-segment negtennegten eightthree))

;; > (print-point (midpoint-segment littlehorizseg))
;; (6,3)
;; > (print-point (midpoint-segment littlediagseg))
;; (-5,-5)
;; > (print-point (midpoint-segment bigdiagseg))
;; (-1,-7/2)

;;;; 2.3
;; Implement a representation for rectangles in a plane.  (Hint: You
;; may want to make use of exercise 2.2.)  In terms of your
;; constructors and selectors, create procedures that compute the
;; perimeter and the area of a given rectangle.  Now implement a
;; different representation for rectangles.  Can you design your
;; system with suitable abstraction barriers, so that the same
;; perimeter and area procedures will work using either
;; representation?

;; Hmm, okay.  How can we implement rectangles?  How about with the
;; bottom left and top right corners?

(define make-rectangle
  (lambda (blpt trpt)
    (cons blpt trpt)))

(define rect-bl
  (lambda (rect)
    (car rect)))

(define rect-tr
  (lambda (rect)
    (cdr rect)))

;; If bottom left is (a, b) and top right is (c, d), we know that
;; bottom right is (c, b) and top left is (a, d).  Area is base *
;; height, or the distance from (a, b) to (c, b) * the distance from
;; (a, b) to (a, d).  That is, it's (c - a) * (d - b).

(define rect-area
  (lambda (rect)
    (let ([a (x-point (rect-bl rect))]
          [b (y-point (rect-bl rect))]
          [c (x-point (rect-tr rect))]
          [d (y-point (rect-tr rect))])
      (* (- c a) (- d b)))))

;; And perimeter, let's see.  Perimeter is the distance from (a, b) to
;; (a, d) plus the distance from (a, d) to (c, d), times 2.  So 2 *
;; ((d - b) + (c - a)).

(define rect-perim
  (lambda (rect)
    (let ([a (x-point (rect-bl rect))]
          [b (y-point (rect-bl rect))]
          [c (x-point (rect-tr rect))]
          [d (y-point (rect-tr rect))])
      (* 2 (+ (- d b) (- c a))))))

;; todo: tests and alternate representation.

;;;; 2.32
;; We can represent a set as a list of distinct elements, and we can
;; represent the set of all subsets of the set as a list of lists. For
;; example, if the set is (1 2 3), then the set of all subsets is (()
;; (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3)). Complete the following
;; definition of a procedure that generates the set of subsets of a
;; set and give a clear explanation of why it works:

;; This works with the insight that when we're generating a given
;; subset, we have to decide whether a particular element is going to
;; be in it, or not. The element in question will be the car of the
;; list -- we say, okay, I'll have generated all the subsets that
;; don't contain the head; we'll call that "rest". Now I just have to
;; make all the subsets that /do/ contain the head. But that's just
;; all the ones that don't, with the head tacked on the front. Append
;; the two together, and we win.

(define (subsets s)
  (if (null? s)
      (list '())
      (let ((rest (subsets (cdr s))))
        (append rest
                (map (lambda (sub) (cons (car s) sub))
                     rest)))))

;;;; 2.33
;; Fill in the missing expressions to complete the following
;; definitions of some basic list-manipulation operations as
;; accumulations:

;; accumulate here is foldr.
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (my-map p sequence)
  (accumulate (lambda (x y)
                (cons (p x) y))
              nil sequence))

(define (my-append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (my-length sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))
