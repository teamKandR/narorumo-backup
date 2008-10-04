;; Structure and Interpretation of Computer Programs Exercises
;; Chapter 2
;; Lindsey Kuper and Alex Rudnick


;; Did we have to define nil somewhere else? PLT doesn't seem to have it by
;; default.
(define nil '())

;;;; 2.32
;; We can represent a set as a list of distinct elements, and we can represent
;; the set of all subsets of the set as a list of lists. For example, if the set
;; is (1 2 3), then the set of all subsets is (() (3) (2) (2 3) (1) (1 3) (1 2)
;; (1 2 3)). Complete the following definition of a procedure that generates the
;; set of subsets of a set and give a clear explanation of why it works:


;; This works with the insight that when we're generating a given subset, we
;; have to decide whether a particular element is going to be in it, or not. The
;; element in question will be the car of the list -- we say, okay, I'll have
;; generated all the subsets that don't contain the head; we'll call that
;; "rest". Now I just have to make all the subsets that /do/ contain the
;; head. But that's just all the ones that don't, with the head tacked on the
;; front. Append the two together, and we win.

(define (subsets s)
  (if (null? s)
      (list '())
      (let ((rest (subsets (cdr s))))
        (append rest
		(map (lambda (sub) (cons (car s) sub))
		     rest)))))

;;;; 2.33
;; Fill in the missing expressions to complete the following definitions of some
;; basic list-manipulation operations as accumulations:


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