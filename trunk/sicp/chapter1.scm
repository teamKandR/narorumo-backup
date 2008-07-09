;; Structure and Interpretation of Computer Programs Exercises
;; Chapter 1
;; Lindsey Kuper and Alex Rudnick

;;;; 1.1
;; What is the result printed by the interpreter in response to each
;; expression? ...

;: 10
; > 10

;: (+ 5 3 4)
; > 12

;: (- 9 1)
; > 8

;: (/ 6 2)
; > 3

;: (+ (* 2 4) (- 4 6))
; > 6

;: (define a 3)
; nothing

;: (define b (+ a 1))
; nothing

;: (+ a b (* a b))
; > 19

;: (= a b)
; > #f

;: (if (and (> b a) (< b (* a b)))
;:     b
;:     a)
; > 4

;: (cond ((= a 4) 6)
;:       ((= b 4) (+ 6 7 a))
;:       (else 25))
; > 16

;: (+ 2 (if (> b a) b a))
; > 6

;: (* (cond ((> a b) a)
;: 	 ((< a b) b)
;: 	 (else -1))
;:    (+ a 1))
; > 16

;;;; 1.2
;; Translate the following expression into prefix form.

(define (one-point-two)
  (/ (+ 5 4 (- 2 (- 3 (+ 6 4/5))))
     (* 3 (- 6 2) (- 2 7))))

;;;; 1.3
;; Define a procedure that takes three numbers as arguments and returns
;; the sum of squares of the two larger numbers.

(define (square x) (* x x))

(define (sum-of-squares a b)
  (+ (square a) (square b)))

(define (sum-of-squares-of-largest-two a b c)
  (cond
    ;; a is the smallest.
    ((and (< a b) (< a c))
     (print 12)
     (sum-of-squares b c))
    
    ;; b is the smallest.
    ((and (< b a) (< b c))
     (sum-of-squares a c))
    
    ;; otherwise c is the smallest.
    (#t
     (sum-of-squares a b))))

;;;; 1.4
;; Observe that our model of evaluation allows for combinations whose
;; operators are compound expressions. Use this observation to describe the
;; behavior of the following procedure:
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

;; The body of the function takes the form:
;; ( (expression that evaluates to a function) a b)
;; ... and the value of the expression is either + or -, such that we always
;; add the absolute value of b to a.

;;;; 1.5
;; Ben Bitdiddle defines the following procedures...

(define (p) (p))
(define (test x y)
  (if (= x 0)
      0
      y))

;; He then evaluates the expression (test 0 (p)).
;; What behavior will Ben observe with an interpreter that uses
;; applicative-order evaluation? What behavior will he observe with an
;; interpreter that uses normal-order evaluation? ...

;; alexr: In the applicative-order case, the interpreter should freeze up and
;; not return any value at all; in order to evaluate the expression (test 0
;; (p)), we would find the value of each part of the expression first --
;; yielding, in order, a procedure, 0... and a tail-recursive infinite loop.

;; The normal-order case (** XXX: is this the same as lazy evaluation, like in
;; Haskell? Or is it like thunks and call-by-name? Or are these the same
;; thing?
;; **) would expand out the expression to be (if (= 0 0) 0 (p)), and evaluate
;; out to 0, never jumping into the infinite tail recursion.

;;;; 1.6
;; Alyssa P. Hacker doesn't see why /if/ needs to be provided as a special
;; form...
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

;; (previously defined functions, needed here)
(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt-new-if x)
  (sqrt-iter-new-if 1.0 x))

;; Delighted, Alyssa uses new-if to rewrite the square root program:
(define (sqrt-iter-new-if guess x)
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter (improve guess x)
                     x)))
;; What happens when Alyssa attempts to use this to compute square roots?

;; alexr: Eva Lu Ator has good intentions, but until we figure out how to use
;; the macro system (Scheme has a macro system, yes? We could write this in
;; Common Lisp macros, anyway) our expressions still get evaluated in
;; applicative order, including this function new-if. This means that when we
;; evaluate, for example, (sqrt-iter 1 25), all three arguments to new-if are
;; evaluated. Thus, irrespective of whether the guess is good enough, we will
;; recursively call sqrt-iter again with an improved guess. So with new-if,
;; sqrt-iter will never return (but it shouldn't blow the stack, either, being
;; tail-recursive.)

;;;; 1.7
;; The /good-enough?/ test used in computing square roots will not be very
;; effective for finding the square roots of very small numbers. Also, in real
;; computers, arithmetic operations are almost always performed with limited
;; precision. This makes our test inadequate for very large numbers. Explain
;; these statements, with examples showing how the test fails for small and
;; large numbers. An alternative strategy for implementing /good-enough?/ is
;; to watch how /guess/ changes from one iteration to the next and to stop
;; when the change is a very small faction of the guess. Design a square-root
;; procedure that uses this kind of test. Does this work better for small and
;; large numbers?
(define (sqrt-iter guess x)
  (print guess)
  (newline)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

;(define (sqrt x)
; (sqrt-iter 1.0 x))

;; alexr: Well, in the case of small numbers, once we're working at a small
;; enough order of magnitude, all numbers will be marked as Good Enough. For
;; example, the square root of 1e-20 is really 1e-10. But 0.01 (eg, 1e-2) is
;; marked by /good-enough?/ as an acceptable approximation of its sqrt -- at 8
;; orders of magnitude off. (sqrt 1e-20) is calculated as 0.03125, with our
;; current code.

;; alexr: For very large numbers, the problem deals with the floating-point
;; representation of numbers in the computer -- they aren't
;; arbitrary-precision. Particularly, in the case of trying to compute the
;; sqrt of 3e20, the procedure (on mzscheme on amd64, anyway) quickly
;; converges 17320508075.688774, which is pretty close to the sqrt of 3e20,
;; but not good-enough, as such. Importantly, the improve function doesn't
;; move it at all; there are only so many bits to represent numbers any closer
;; to the actual sqrt. So in this case, the call never terminates...

(define (small-change? prevguess guess)
  "Is the difference less than 1% of guess?"
  (< (/ (abs (- prevguess guess)) guess) 0.01))

(define (better-sqrt x)  
  (define (sqrt-iter prevguess guess)
    (print guess)
    (newline)
    (if (small-change? prevguess guess)
        guess
        (sqrt-iter guess (improve guess x))))
  (sqrt-iter 0.0 1.0))

;;;; 1.8
;; Newton's method for cube roots is based on the fact that if y is an
;; approximation to the cube root of x,  then a better approximation is given
;; by the value (/ (+ (/ x (expt y 2)) (* 2 y)) 3).
;; Use this formula to implement a cube-root procedure analogous to the
;; square-root procedure.

(define (cbrt-improve guess x)
  (/ (+ (/ x (expt guess 2)) (* 2 guess)) 3))

(define (newton-cbrt x)
  (define (cbrt-iter prevguess guess)
    (if (small-change? prevguess guess)
        guess
        (cbrt-iter guess (cbrt-improve guess x))))
  
  (cbrt-iter 0.0 1.0))

;;;; 1.9
;; Each of the following two procedures defines a method for adding two
;; positive integers in terms of the procedures inc and dec.

(define (inc a) (+ a 1))
(define (dec a) (- a 1))

(define (plus-v1 a b)
  (if (= a 0)
      b
      (inc (plus-v1 (dec a) b))))

(define (plus-v2 a b)
  (if (= a 0)
      b
      (plus-v2 (dec a) (inc b))))


;; Using the substitution model, illustrate the process generated by each
;; procedure in evaluating (+ 4 5). Are these processes iterative or
;; recursive?

;; alexr: The first procedure, in evaluating (plus-v1 4 5), becomes:

;; (plus-v1 4 5)
;; (inc (plus-v1 3 5))
;; (inc (inc (plus-v1 2 5)))
;; (inc (inc (inc (plus-v1 1 5))))
;; (inc (inc (inc (inc (plus-v1 0 5)))))
;; (inc (inc (inc (inc 5))))
;; (inc (inc (inc 6)))
;; (inc (inc 7))
;; (inc 8)
;; 9.

;; ... which is to say, all of the plus-v1 stack frames have to be
;; remembered. It's a recursive process.

;; Contrastingly, for plus-v2, we get:
;; (plus-v2 4 5)
;; (plus-v2 3 6)
;; (plus-v2 2 7)
;; (plus-v2 1 8)
;; (plus-v2 0 9)
;; 9.

;; Iterative.

;;;; 1.10
;; Ackermann's function:
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        
        (else (A (- x 1)
                 (A x (- y 1))))))

;; What are the values of the following expressions?
;; (A 1 10)
;; 1024
;; (A 2 4)
;; 65536
;; (A 3 3)
;; 65536.

(define (f n) (A 0 n))
(define (g n) (A 1 n))
(define (h n) (A 2 n))

;; Give concise mathematical definitions for the functions computed by the
;; procedures f, g, and h for positive integer values of n.

;; alexr: (f n) doubles the input. Since f passes 0 as the first argument to
;; A,
;; it triggers the second case, which doubles the y argument.

;; (g n) comes out to be 2^n, expanding out to a bunch of nested
;; (* 2 (* 2 ... y)). In fact, exactly n doublings.

;; (h n), well, let's do an expansion.

;; (h 4)
;; (A 2 4)
;; (A 1 (A 2 3))
;; (A 1 (A 1 (A 2 2)))
;; (A 1 (A 1 (A 1 (A 2 1))))
;; (A 1 (A 1 (A 1 2)))
;; (A 1 (A 1 (A 0 (A 1 1))))
;; (A 1 (A 1 (A 0 2)))
;; (A 1 (A 1 4))
;; ... TODO(alexr).

;; lindseykuper:
;; well, if f(n) = 2n
;; and
;; g(n) = n^2
;; maybe h(n) will be, uh, n^n?
;; okay, empirically no, because h(1) = 2.
;; 2, 4, 16, 65536. 2^(n^2) ?
;; no, because h(2) = 4, not 16.  We need something that doesn't expand
;; *quite* so fast.

;; Let's try a mathier approach:
;; We ought to be able to define h in terms of g,
;; and g in terms of f:

(define more-verbose-g
  (lambda (n)
    (f (g (- n 1)))))

(define more-verbose-h
  (lambda (n)
    (g (h (- n 1)))))

(define even-more-verbose-h
  (lambda (n)
    ((lambda (n)
       (f (g (- n 1))))
     (h (- n 1)))))

;; That's interesting, but it hasn't gotten me anywhere just yet.
;; Let's do the expansion thing, but keep it in terms of f.
;; (g 4)
;; (f (g 3))
;; (f (f (g 2)))
;; (f (f (f (g 1))))
;; (f (f (f 2)))
;; (f (f 4)) ;; we know that f doubles its input 
;; (f 8)
;; 16

;; Now we have a base case for more-verbose-g.  Woohoo!
(define recursive-verbose-g
  (lambda (n)
    (if (= n 1)
        2
        (f (recursive-verbose-g (- n 1))))))

;; And:

;; (h 4)
;; (g (h 3))
;; (g (g (h 2)))
;; (g (g (g (h 1))))
;; (g (g (g 2)))
;; (g (g 4))
;; (g 16)
;; 65536

;; lindseykuper and alexr: (h n) is the expansion of n (g (g ... 2)) (for n
;; nested instances of /g/) -- which is to say that it's 2^(2^(2 ...)). It's 2
;; with a bunch of exponents going up diagonally, of length n. Win!

;;;; 1.11
;; A function f is defined by the rule that f(n) = n if n<3 and 
;; f(n) = f(n - 1) + 2f(n - 2) + 3f(n - 3) if n >= 3. Write a procedure that
;; computes f by means of a recursive process. Write a procedure that computes
;; f by means of an iterative process.

;; recursive version.

(define (f-recursive n)
  (cond
    ((< n 3) n)
    (else (+ (f-recursive (- n 1))
             (* 2 (f-recursive (- n 2)))
             (* 3 (f-recursive (- n 3)))))))


;; alexr: the iterative version took some thinking, and I wrote it in Python
;; first. This looks pretty scheme-able!

;; def f_iterative(n):
;;   oneago = min(n-1, 2)
;;   twoago = min(n-2, 1)
;;   threeago = min(n-3, 0)
;;   sofar = min(2, n)
;;
;;   for i in xrange(3, n + 1):
;;     sofar = oneago + 2 * twoago + 3 * threeago
;;
;;     threeago = twoago
;;     twoago = oneago
;;     oneago = sofar

;;   return sofar


;; lindseykuper and alexr: Python v2
;def f_iterative(n):
;  if (n < 3):
;    return n
;
;  oneago = 2
;  twoago = 1
;  threeago = 0
;  sofar = 2
;
;  for i in xrange(3, n + 1):
;    sofar = oneago + 2 * twoago + 3 * threeago
;
;    threeago = twoago
;    twoago = oneago
;    oneago = sofar
;
;  return sofar

;; lindseykuper: iterative Scheme version.

(define f-iter
  (lambda (n)
    (letrec ((f-iter-kernel
              (lambda (n oneago twoago threeago i)
                (let ((sofar (+ oneago (* 2 twoago) (* 3 threeago))))
                  (if (= n i)
                      sofar
                      (f-iter-kernel n sofar oneago twoago (+ i 1)))))))
      (if (< n 3)
          n
          (f-iter-kernel n 2 1 0 3)))))

;;;; 1.12
;; ... Write a procedure that computes elements of Pascal's triangle by means
;; of a recursive process. 

;;       1
;;      1 1
;;     1 2 1
;;    1 3 3 1
;;   1 4 6 4 1
;;  1 5 A A 5 1

;; Yoshi! ("Do it!", roughly)

;; alexr: Base case: if it's "on the edge", it's a 1.
;; The ith row is i+1 elements wide. So an element is On The Edge if its index
;; is 0 or i.

;; lindseykuper: So, to get (row 3, col 1), which is supposed to be 3,
;; check if col == 0 or 3.  It's not.
;; So, then, look at row "row-1", cols "col-1" and "col".  The sum of those is
;; the answer.

;; The most that row can ever be is col-1.

(define pascals-triangle
  ;"Returns the value of one element in Pascal's Triangle at specified
  ; coordinates."
  (lambda (row col)
    (cond
      ((or (= col 0) (= col row)) 1)
      ((> col row) 'dontdothat) 
      (else 
       (+ (pascals-triangle (- row 1) (- col 1))
          (pascals-triangle (- row 1) col))))))

;;;; 1.14
;; Draw the tree illustrating the process generated by the /count-change/ 
;; procedure of section 1.2.2 in making change for 11 cents.

;; lindseykuper: Let's try this again, and try to be a little cleaner about it!

;(count-change 11)
;
;(cc 11 5)
;
;(+ (cc 11 (- 5 1)) (cc (- 11 50) 5))
;
;(+ (cc 11 4) (cc -39 5))
;
;(+ (+ (cc 11 (- 4 1)) (cc (- 11 25) 4)) 0)
;
;(+ (+ (cc 11 3) (cc -14 4)) 0)
;
;(+ (+ (+ (cc 11 (- 3 1)) (cc (- 11 10) 3)) 0) 0)
;
;(+ (+ (+ (cc 11 2) (cc 1 3)) 0) 0)
;
;(+ (+ (+ (+ (cc 11 (- 2 1)) (cc (- 11 5) 2)) 
;         (+ (cc 1 (- 3 1)) (cc (- 1 10) 3))) 0) 0)
;
;(+ (+ (+ (+ (cc 11 1) (cc 6 2)) (+ (cc 1 2) (cc -9 3))) 0) 0)
;
;(+ (+ (+ (+ (+ (cc 11 (- 1 1)) (cc (- 11 1) 1)) 
;            (+ (cc 6 (- 2 1)) (cc (- 6 5) 2))) 
;         (+ (+ (cc 1 (- 2 1)) (cc (- 1 5) 2)) 0)) 0) 0)
;
;(+ (+ (+ (+ (+ (cc 11 0) (cc 10 1)) (+ (cc 6 1) (cc 1 2))) 
;         (+ (+ (cc 1 1) (cc -4 2)) 0)) 0) 0)
;
;(+ (+ (+ (+ (+ 0 (cc 10 1)) 
;            (+ (cc 6 1) (cc 1 2))) (+ (+ (cc 1 1) 0) 0)) 0) 0)
;
;(+ (+ (+ (+ (+ 0 (+ (cc 10 (- 1 1)) (cc (- 10 1) 1))) 
;            (+ (+ (cc 6 (- 1 1)) (cc (- 6 1) 1)) 
;               (+ (cc 1 (- 2 1)) (cc (- 1 5) 2)))) 
;         (+ (+ (+ (cc 1 (- 1 1)) (cc (- 1 1) 1)) 0) 0)) 0) 0)
;
;(+ (+ (+ (+ (+ 0 (+ (cc 10 0) (cc 9 1))) 
;            (+ (+ (cc 6 0) (cc 5 1)) 
;               (+ (cc 1 1) (cc -4 2)))) 
;         (+ (+ (+ (cc 1 0) (cc 0 1)) 0) 0)) 0) 0)
;
;(+ (+ (+ (+ (+ 0 (+ 0 (cc 9 1))) 
;            (+ (+ 0 (cc 5 1)) 
;               (+ (cc 1 1) (cc -4 2)))) 
;         (+ (+ (+ 0 1) 0) 0)) 0) 0)
;
;(+ (+ (+ (+ (+ 0 (+ 0 (cc 9 1))) (+ (+ 0 (cc 5 1)) (+ (cc 1 1) 0)))
;         (+ (+ 1 0) 0)) 0) 0)
;
;(+ (+ (+ (+ (+ 0 (+ 0 (cc 9 1))) (+ (+ 0 (cc 5 1)) (+ (cc 1 1) 0))) 1) 0) 0)
;
;(+ (+ (+ (+ (+ 0 (+ 0 (+ (cc 9 (- 1 1)) (cc (- 9 1) 1)))) 
;            (+ (+ 0 (+ (cc 5 (- 1 1)) (cc (- 5 1) 1))) 
;               (+ (+ (cc 1 (- 1 1)) (cc (- 1 1) 1)) 0))) 1) 0) 0)
;
;(+ (+ (+ (+ (+ 0 (+ 0 (+ (cc 9 0) (cc 8 1)))) 
;            (+ (+ 0 (+ (cc 5 0) (cc 4 1))) 
;               (+ (+ (cc 1 0) (cc 0 1)) 0))) 1) 0) 0)
;
;(+ (+ (+ (+ (+ 0 (+ 0 (+ 0 (cc 8 1)))) 
;            (+ (+ 0 (+ 0 (cc 4 1))) 
;               (+ (+ 0 1) 0))) 1) 0) 0)
;
;(+ (+ (+ (+ (+ 0 (+ 0 (+ 0 (cc 8 1)))) (+ (+ 0 (+ 0 (cc 4 1))) 1)) 1) 0) 0)
;
;(+ (+ (+ (+ (+ 0 (+ 0 (+ 0 (+ (cc 8 (- 1 1)) (cc (- 8 1) 1))))) 
;            (+ (+ 0 (+ 0 (+ (cc 4 (- 1 1)) (cc (- 4 1) 1)))) 1)) 1) 0) 0)
;
;(+ (+ (+ (+ (+ 0 (+ 0 (+ 0 (+ (cc 8 0) (cc 7 1))))) 
;            (+ (+ 0 (+ 0 (+ (cc 4 0) (cc 3 1)))) 1)) 1) 0) 0)
;
;(+ (+ (+ (+ (+ 0 (+ 0 (+ 0 (+ 0 (cc 7 1))))) 
;            (+ (+ 0 (+ 0 (+ 0 (cc 3 1)))) 1)) 1) 0) 0)
;
;(+ (+ (+ (+ (+ 0 (+ 0 (+ 0 (+ 0 (cc 7 1))))) 
;            (+ (+ 0 (+ 0 (+ 0 (cc 3 1)))) 1)) 1) 0) 0)
;
;(+ (+ (+ (+ (+ 0 (+ 0 (+ 0 (+ 0 (+ (cc 7 (- 1 1)) (cc (- 7 1) 1)))))) 
;            (+ (+ 0 (+ 0 (+ 0 (+ (cc 3 (- 1 1)) (cc (- 3 1) 1))))) 1)) 
;         1) 0) 0)
;
;(+ (+ (+ (+ (+ 0 (+ 0 (+ 0 (+ 0 (+ (cc 7 0) (cc 6 1)))))) 
;            (+ (+ 0 (+ 0 (+ 0 (+ (cc 3 0) (cc 2 1))))) 1)) 1) 0) 0)
;
;(+ (+ (+ (+ (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (cc 6 1)))))) 
;            (+ (+ 0 (+ 0 (+ 0 (+ 0 (cc 2 1))))) 1)) 1) 0) 0)
;
;(+ (+ (+ (+ (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (+ (cc 6 (- 1 1)) 
;                                        (cc (- 6 1) 1))))))) 
;            (+ (+ 0 (+ 0 (+ 0 (+ 0 (+ (cc 2 (- 1 1)) 
;                                      (cc (- 2 1) 1)))))) 1)) 1) 0) 0)
;
;(+ (+ (+ (+ (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (+ (cc 6 0) (cc 5 1))))))) 
;            (+ (+ 0 (+ 0 (+ 0 (+ 0 (+ (cc 2 0) (cc 1 1)))))) 1)) 1) 0) 0)
;
;(+ (+ (+ (+ (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (cc 5 1))))))) 
;            (+ (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (cc 1 1)))))) 1)) 1) 0) 0)
;
;(+ (+ (+ (+ (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (cc 5 1))))))) 
;            (+ (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (cc 1 1)))))) 1)) 1) 0) 0)
;
;(+ (+ (+ (+ (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (+ (cc 5 (- 1 1)) 
;                                             (cc (- 5 1) 1)))))))) 
;            (+ (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (+ (cc 1 (- 1 1)) 
;                                           (cc (- 1 1) 1))))))) 1)) 1) 0) 0)
;
;(+ (+ (+ (+ (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (+ (cc 5 0) (cc 4 1)))))))) 
;            (+ (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (+ (cc 1 0) 
;                                           (cc 0 1))))))) 1)) 1) 0) 0)

; ``He said, `It's no use, I only know half;
;   no speak the English; only do the math.' ''

;(+ (+ (+ (+ (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (cc 4 1)))))))) 
;            (+ (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 
;                                           1)))))) 1)) 1) 0) 0)
;
;(+ (+ (+ (+ (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (cc 4 1)))))))) 
;            (+ (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 1))))) 1)) 1) 0) 0)
;
;(+ (+ (+ (+ (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (cc 4 1)))))))) 
;            (+ (+ 0 (+ 0 (+ 0 (+ 0 1)))) 1)) 1) 0) 0)
;
;(+ (+ (+ (+ (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (cc 4 1)))))))) 
;            (+ (+ 0 (+ 0 (+ 0 1))) 1)) 1) 0) 0)
;
;(+ (+ (+ (+ (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (cc 4 1)))))))) 
;            (+ (+ 0 (+ 0 1)) 1)) 1) 0) 0)
;
;(+ (+ (+ (+ (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (cc 4 1)))))))) 
;            (+ (+ 0 1) 1)) 1) 0) 0)
;
;(+ (+ (+ (+ (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (cc 4 1)))))))) 
;            (+ 1 1)) 1) 0) 0)
;
;(+ (+ (+ (+ (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (cc 4 1)))))))) 
;            2) 1) 0) 0)
;
;(+ (+ (+ (+ (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (+ (cc 4 (- 1 1)) 
;                                                  (cc (- 4 1) 1))))))))) 
;            2) 1) 0) 0)
;
;(+ (+ (+ (+ (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (+ (cc 4 0) 
;                                                  (cc 3 1))))))))) 
;            2) 1) 0) 0)
;
;(+ (+ (+ (+ (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 
;                                                  (cc 3 1))))))))) 
;            2) 1) 0) 0)
;
;(+ (+ (+ (+ (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 
;                                                  (+ (cc 3 (- 1 1)) 
;                                                     (cc (- 3 1) 1)
;                                                     ))))))))) 
;            2) 1) 0) 0)
;
;(+ (+ (+ (+ (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 
;                                                  (+ (cc 3 0) 
;                                                     (cc 2 1)
;                                                     ))))))))) 
;            2) 1) 0) 0)
;
;(+ (+ (+ (+ (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 
;                                                       (cc 2 1)
;                                                       ))))))))) 
;            2) 1) 0) 0)
;
;(+ (+ (+ (+ (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 
;                                                       (+ (cc 2 (- 1 1)) 
;                                                          (cc (- 2 1)
;                                                              1))))))))))) 
;            2) 1) 0) 0)
;
;(+ (+ (+ (+ (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 
;                                                       (+ (cc 2 0) 
;                                                          (cc 1 1)
;                                                          )))))))))) 
;            2) 1) 0) 0)
;
;(+ (+ (+ (+ (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 
;                                                            (+ 
;                                                             (cc 1 (- 1 1)) 
;                                                               (cc (- 1 1)
;                                                                   1)
;                                                               ))))))))))) 
;            2) 1) 0) 0)
;
;(+ (+ (+ (+ (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 
;                                                            (+ (cc 1 (- 1 1)) 
;                                                               (cc (- 1 1)
;                                                                   1)
;                                                               ))))))))))) 
;            2) 1) 0) 0)
;
;(+ (+ (+ (+ (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 
;                                                            (+ (cc 1 0) 
;                                                               (cc 0 1)
;                                                               ))))))))))) 
;            2) 1) 0) 0)
;
;(+ (+ (+ (+ (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 1)
;                                                            )))))))))) 
;            2) 1) 0) 0)
;
;(+ (+ (+ (+ (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 1)
;                                                       ))))))))) 
;            2) 1) 0) 0)
;
;(+ (+ (+ (+ (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 1))))))))) 
;            2) 1) 0) 0)
;
;(+ (+ (+ (+ (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 1)))))))) 2) 1) 0) 0)
;
;(+ (+ (+ (+ (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 1))))))) 2) 1) 0) 0)
;
;(+ (+ (+ (+ (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 1)))))) 2) 1) 0) 0)
;
;(+ (+ (+ (+ (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 1))))) 2) 1) 0) 0)
;
;(+ (+ (+ (+ (+ 0 (+ 0 (+ 0 (+ 0 1)))) 2) 1) 0) 0)
;
;(+ (+ (+ (+ (+ 0 (+ 0 (+ 0 1))) 2) 1) 0) 0)
;
;(+ (+ (+ (+ (+ 0 (+ 0 1)) 2) 1) 0) 0)
;
;(+ (+ (+ (+ (+ 0 1) 2) 1) 0) 0)
;
;(+ (+ (+ (+ 1 2) 1) 0) 0)
;
;(+ (+ (+ 3 1) 0) 0)
;
;(+ (+ 4 0) 0)
;
;(+ 4 0)
;
;4

; ...*falls over*

;;; optimization idea: test first to see whether /amount/ is less than any of 
;;; the denominations, and only use denominations that are less then or equal 
;;; to /amount/.

;;; another optimization idea: if /amount/ is less than 5 and /kinds-of-coins/
;;; is 1, return 1.

;;;; 1.16
;; Design a procedure that evolves an iterative exponentiation process that 
;; uses successive squaring and uses a logarithmic number of steps, as does 
;; /fast-expt/.

;; lindseykuper: The idea is that we'll work from the bottom up and keep 
;; adding stuff to /a/ until /a/ is the answer.  /n/ will keep decreasing.  
;; /ab^n/ is the invariant.

;; ab^n = the answer.
;; a = 1 initially.
;; in the transformation, let a = ab^(n/2)
;; and let b^n = b^(n/2).
;; n will equal 0 and b^n will equal 1 by the time we're done.

;; lindseykuper: No, I take it back!  The trouble with doing a = ab^(n/2) is
;; that, in every step, we have to calculate another exponent to a power that
;; we don't know yet.  So this can never be entirely iterative, because we'll
;; keep on stacking up recursive calls to fast-expt-iter so that we can find
;; b^(n/2).

;; However!  
;; We know that: b^n = b^(n/2) * b^(n/2) = (b^(n/2))^2 = (b^2)^(n/2).

;; So, (fast-expt-iter b n) ought to be the same as 
;; (fast-expt-iter (square b) (/ n 2)).

;; Why can't we just do this?

;; (define fast-expt-iter
;;   (lambda (b n)
;;   (fast-expt-iter (square b) (/ n 2))))

;; Well, that would be great if  /n/ were always even (i.e., if it were a power
;; of 2 initially).  But we can't guarantee that.  So we need the state
;; variable /a/ *just* to handle cases where n is odd.

(define fast-expt-iter
  (lambda (b n)
    (fast-expt-iter-kernel b n 1)))

(define fast-expt-iter-kernel
  (lambda (b n a)
    (cond ((= n 0) a)
          ((even? n) (fast-expt-iter-kernel (square b) (/ n 2) a))
          (else (fast-expt-iter-kernel b (- n 1) (* a b))))))

; Test cases:
;(fast-expt-iter 2 2)     ; 4
;(fast-expt-iter 2 5)     ; 32
;(fast-expt-iter 2 30)    ; 1073741824
;(fast-expt-iter 2 10000) ; ph33r my l33t sp33d.

;;;; 1.17
;; ...design a multiplication procedure analogous to /fast-expt/ that uses a 
;; logarithmic number of steps.

(define double
  (lambda (n)
    (+ n n)))

(define halve
  (lambda (n)
    (if (even? n)
        (/ n 2)
        (error "That number wasn't even!"))))

(define fast-mult
  (lambda (a b)
    (cond 
      ; one or both of the operands is 0
      ((or (= a 0) (= b 0)) 0) 
      ; exactly one of the operands is negative
      ((xor (< a 0) (< b 0)) (- (fast-mult-kernel (abs a) (abs b))))
      ; both operands are of the same sign
      (else (fast-mult-kernel (abs a) (abs b))))))

(define fast-mult-kernel
  (lambda (a b)
    (cond ((= a 1) b)
          ((even? a) (fast-mult-kernel (halve a) (double b)))
          (else (+ b (fast-mult-kernel (- a 1) b))))))

;;;; 1.18
;; ...devise a procedure that generates an iterative process for multiplying
;; two integers in terms of adding, doubling, and halving and uses a 
;; logarithmic number of steps.

;; lindseykuper:
;; The idea is that (a * b) is equal to ((a/2) * 2b), assuming /a/ is even. 
;; So, test if a = 1, and if so, we're done.  Otherwise, on each iteration, 
;; test for /a/'s evenness.  If /a/ is odd, subtract 1 from /a/ and add /b/ to
;; the accumulator (which again only exists to deal with odd cases).  If /a/ 
;; is even, halve /a/, double /b/, and call the procedure again with the 
;; resulting values.  The final answer will be /b/ plus whatever's accumulated
;; in the accumulator.

;; lindseykuper: I decided we needed to define xor.
(define xor 
  (lambda (left right)
    (or (and left (not right)) (and right (not left)))))

(define fast-mult-iter
  (lambda (a b)
    (cond 
      ; one or both of the operands is 0
      ((or (= a 0) (= b 0)) 0) 
      ; exactly one of the operands is negative
      ((xor (< a 0) (< b 0)) (- (fast-mult-iter-kernel (abs a) (abs b) 0)))
      ; both operands are of the same sign
      (else (fast-mult-iter-kernel (abs a) (abs b) 0)))))

(define fast-mult-iter-kernel
  (lambda (a b accumulator)
    (cond ((= a 1) (+ b accumulator))
          ((even? a) (fast-mult-iter-kernel (halve a) (double b) accumulator))
          (else (fast-mult-iter-kernel (- a 1) b (+ b accumulator))))))

;;;; 1.19
;; (logarithmic procedure for computing Fib(n))

;; The transformation T is when you do 
;; T = {
;;       a -> a + b
;;       b -> a
;; }
;;
;; T is a special case of T_pq where p = 0 and q = 1.
;; T_pq = {  
;;      a -> bq + aq + ap
;;      b -> bp + aq
;; }
;;
;; Okay, what happens when we apply T_pq twice, in general?
;; a = bq + aq + ap 
;; b = bp + aq
;; new a = (bp + aq)q + (bq + aq + ap)q + (bq + aq + ap)p
;; new b = (bp + aq)p + (bq + aq + ap)q
;; simplified:
;; new a = 2bpq + 2apq + 2(aq^2) + bq^2 + ap^2
;; new b = bp^2  + bq^2 + aq^2 + 2apq
;; 
;; So, we've got:
;;
;; T_pq_TWICE = {
;;      a -> 2bpq + 2apq + 2(aq^2) + bq^2 + ap^2
;;      b -> bp^2  + bq^2 + aq^2 + 2apq
;; }
;;
;; I think what we have to do now is figure out how what we have above fits
;; into the T_pq mold.  That is, we have to figure out what /p/ and /q/ have
;; to be to make the T_pq_TWICE transformation happen.

;; bq' + aq' + ap' = 2bpq + 2apq + 2(aq^2) + bq^2 + ap^2      (1)
;; bp' + aq'       = bp^2  + bq^2 + aq^2 + 2apq               (2)

;; Subtract equation (2) from equation (1):
;; ap' + bq' - bp'    = ap^2 + 2bpq  + aq^2 - bp^2
;; a(p') + b(q' - p') = a(p^2 + q^2) + b(2pq - p^2)
;;       p' + q' - p' = p^2 + q^2 + 2pq - p^2
;;                 q' = q^2 + 2pq

;; and then
;; bp' + a(q^2 + 2pq) = bp^2  + bq^2 + aq^2 + 2apq  
;; bp' + aq^2 + 2apq  = bp^2  + bq^2 + aq^2 + 2apq  
;;                bp' = bp^2  + bq^2
;;                 p' = p^2 + q^2

;; Do those values for /p'/ and /q'/ check out?

;; a = bq + aq + ap
;; a = b(q^2 + 2pq) + a(q^2 + 2pq) + a(p^2 + q^2)
;; a = bq^2 + 2bpq + aq^2 + 2apq + ap^2 + aq^2 
;; (yes, this equals what we have in T_pq_TWICE.)

;; b = bp + aq
;; b = b(p^2 + q^2) + a(q^2 + 2pq)
;; b = bp^2 + bq^2 + aq^2 + 2apq
;; (yes, this equals what we have in T_pq_TWICE.)

;; Great!  So our procedure:

(define (fib n)
  (fib-iter 1 0 0 1 n))

(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (square p) (square q))      ; compute p'
                   (+ (square q) (* 2 p q))       ; compute q'
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))

; Fib test cases:
;(fib 13)      ; 233
;(fib 20)      ; 6765
;(fib 1000000) ; big, but it looks like it matches what this person found:
               ; http://www.upl.cs.wisc.edu/~bethenco/fibo/

;;;; 1.20
;; ...illustrate the process generated in evaluating /(gcd 206 40)/ and
;; indicate the /remainder/ operations that are actually performed.

;; Applicative order: "evaluate the arguments and then apply"
;(gcd 206 40)                  ; eval
;(gcd 40 (remainder 206 40))   ; apply
;(gcd 40 6)                    ; eval
;(gcd 6 (remainder 40 6))      ; apply
;(gcd 6 4)                     ; eval
;(gcd 4 (remainder 6 4))       ; apply
;(gcd 4 2)                     ; eval
;(gcd 2 (remainder 4 2))       ; apply
;(gcd 2 0)                     ; eval
;2                             ; result

;; Normal order: "fully expand and then reduce"
;(gcd 206 40) ; expand
;(gcd 40 (remainder 206 40)) ; expand
;(gcd (remainder 206 40) (remainder 40 (remainder 206 40))) ; expand
;
;; expand
;(gcd (remainder 40 (remainder 206 40)) 
;     (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))) 
;
;; second argument to gcd finally equals 0
;(gcd (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))
;     (remainder (remainder 40 (remainder 206 40)) 
;                (remainder (remainder 206 40) 
;                           (remainder 40 (remainder 206 40))))) 
;
;; finally, an expression made of only primitives
;(remainder (remainder 206 40) (remainder 40 (remainder 206 40))) ; reduce
;
;(remainder 6 (remainder 40 6)) ; reduce
;(remainder 6 4) ; reduce
;2 ; result

;; In the normal-order version, we have to call /remainder/ 28 times!
;; In the applicative-order version, we have to call /remainder/ only 4 times.

;;;; 1.21
;; Use the /smallest-divisor/ procedure to find the smallest divisor of each
;; of the following numbers: 199, 1999, 19999.

;> (smallest-divisor 199)
;199
;> (smallest-divisor 1999)
;1999
;> (smallest-divisor 19999)
;7

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

;;;; 1.22

; make /runtime/ work in drscheme and mzscheme!
(define runtime current-milliseconds) 

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))    

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display " is prime (and it took ")
  (display elapsed-time)
  (display " ms to find out)"))

;; ...write a procedure search-for-primes that checks the primality of
;; consecutive odd integers in a specified range. 

(define search-for-primes
  (lambda (start finish)
    (search-for-primes-kernel start finish (runtime))))

(define search-for-primes-kernel
  (lambda (start finish start-time)
    (if (not (eq? start finish))
        (cond ((even? start) (search-for-primes-kernel (+ start 1)
                                                       finish start-time))
              (else (timed-prime-test start)
                    (search-for-primes-kernel (+ start 1) finish start-time)))
        (begin
          (newline)
          (display "total runtime: ")
          (display (- (runtime) start-time))))))

;; Use your procedure to find the three smallest primes larger than 1000; 
;; larger than 10,000; larger than 100,000; larger than 1,000,000. 

;; lindseykuper:
;; Three smallest primes larger than     1,000:    1009,    1013,    1019
;;   "      "       "      "     "      10,000:   10007,   10009,   10037
;;   "      "       "      "     "     100,000:  100003,  100019,  100043
;;   "      "       "      "     "   1,000,000: 1000003, 1000033, 1000037

;; My /runtime/ isn't showing me very interesting data for these, though.  It
;; doesn't start to be anything but "0 milliseconds" or "1 millisecond" until
;; we get up into the Really Big numbers:

;; > (search-for-primes 10000000 10000020) ; ten million
;; ...
;; 10000019 is prime (and it took 2 ms to find out)

;; > (search-for-primes 10000000 10000020) ; a hundred million
;; ...
;; 100000007 is prime (and it took 9 ms to find out)

;; lindseykuper again:
;; So, the hypothesis is that testing for primes around 10,000 should take 
;; about (sqrt 10) (or roughly 3.16) times as long as testing for primes around
;; 1,000.  But our data *don't* really bear this out.  We don't see much of a
;; change until we get up to primes around 10,000,000.  So, I'm hypothesizing
;; that it's actually *fourth root*?  Or however people say (sqrt (sqrt n)).

;; Hey, Alex!  What do you think?

;; For reference:
;; (sqrt       1,000) =    32 (ish)
;; (sqrt      10,000) =   100
;; (sqrt     100,000) =   316 (roughly)
;; (sqrt   1,000,000) =  1000
;; (sqrt  10,000,000) =  3162 (more or less)
;; (sqrt 100,000,000) = 10000


;; alexr: Well! I think we could do with some more precise numbers, because
;; our computers are so fast, these days. Let's run the prime test a bunch
;; of times in a loop -- it'll be like having a larger sample size.
(define (longer-prime-test n)
  (define (test-kernel n i times)
    (cond
     ((= i times) (prime? n))
     (else (begin
	     (prime? n)
	     (test-kernel n (+ i 1) times)))))

  (let ((start-time (runtime)))
    (if (test-kernel n 0 1000)
	(report-prime (- (runtime) start-time)))))

(define (longer-timed-prime-test n)
  (newline)
  (display n)
  (longer-prime-test n))

(define (longer-search-for-primes start finish)
  (longer-search-for-primes-kernel start finish (runtime)))

(define (longer-search-for-primes-kernel start finish start-time)
  (if (not (eq? start finish))
      (cond ((even? start) (longer-search-for-primes-kernel (+ start 1)
							    finish start-time))
	    (else (longer-timed-prime-test start)
		  (longer-search-for-primes-kernel (+ start 1)
						   finish
						   start-time)))
      (begin
	(newline)
	(display "total runtime: ")
	(display (- (runtime) start-time)))))

;; lindseykuper:
;; You're right.  I don't know why it would be 4th root -- that was a wild
;; stab!  Now I think that the issue is that this problem was written in 1985,
;; and none of the tests take long enough to distinguish.  Using your test, 
;; here's how long it takes to test each of these numbers 1000 times  (I 
;; changed the number of loops to 1000 to make the arithmetic easier):

;;    1009  (19 ms),     1013 (18 ms),     1019 (19 ms)  (avg:  18.67)
;;   10007  (59 ms),    10009 (58 ms),    10037 (59 ms)  (avg:  58.67)
;;  100003 (373 ms),  100019 (199 ms),  100043 (197 ms)  (avg: 256.33)
;; 1000003 (604 ms), 1000033 (597 ms), 1000037 (602 ms)  (avg: 601.00)

;; > (* 18.67 (sqrt 10))
;; 59.03972400882986
;; Pretty close to 58.67 -- so, our data for primes around 10,000 *do* bear out
;; our (sqrt 10) hypothesis.

;; > (* 18.67 (sqrt 100))
;; 186.7000000026119
;; Not super-close to 256.33, but then 373 was an outlier.  This is pretty
;; close to 199 or 197.

;; > (* 18.67 (sqrt 1000))
;; 590.3973483545885
;; Pretty close to 601.

;; So, I feel okay saying that our data support the (sqrt n) prediction Pretty
;; Well.  Whew.  Let's move on to a problem that isn't so machine-dependent!

;;;; 1.23
;; The /smallest-divisor/ procedure shown at the start of this section does
;; lots of needless testing: After it checks to see if the number is divisible
;; by 2 there is no point in checking to see if it is divisible by any larger
;; even numbers. This suggests that the values used for test-divisor should not
;; be 2, 3, 4, 5, 6, ..., but rather 2, 3, 5, 7, 9, .... To implement this
;; change, define a procedure /next/ that returns 3 if its input is equal to 2
;; and otherwise returns its input plus 2. Modify the /smallest-divisor/
;; procedure to use (next test-divisor) instead of (+ test-divisor 1).  With
;; timed-prime-test incorporating this modified version of /smallest-divisor/,
;; run the test for each of the 12 primes found in exercise 1.22. Since this
;; modification halves the number of test steps, you should expect it to run
;; about twice as fast. Is this expectation confirmed? If not, what is the
;; observed ratio of the speeds of the two algorithms, and how do you explain
;; the fact that it is different from 2?

;; alexr: Alright, let's make faster-smallest-divisor and friends. It seems
;; like, if we were really clever, we could have some way for /next/ to just
;; generate the list of prime numbers. But then, if we could build the list of
;; all the prime numbers, testing for primality would be easy -- just check
;; if a number is in that list.

;; By the time we call this function, we know it's not even, and > 2.
(define (faster-smallest-divisor n)
  (faster-find-divisor n 3))

;; lindseykuper:
;; First we tried a version with a /next/ procedure.  Here are the results of
;; my testing that version with our 12 primes from 1.22:

;;    1009  (16 ms),     1013 (13 ms),     1019 (12 ms)  (avg:  13.67)
;;   10007  (36 ms),    10009 (36 ms),    10037 (36 ms)  (avg:  36.00)
;;  100003 (115 ms),  100019 (119 ms),  100043 (115 ms)  (avg: 116.33)
;; 1000003 (371 ms), 1000033 (368 ms), 1000037 (456 ms)  (avg: 398.33)

;; The average times are 73%, 61%, 45%, and 66% of the old ones respectively.
;; I don't think this counts as "twice as fast".

;; alexr points out that every call to /next/ includes a function call and a
;; conditional. Why not just always add 2 instead of asking every single time?

(define (faster-find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (faster-find-divisor n (+ test-divisor 2)))))

;; alexr:
;; Primality test with our new, faster functions. 2 is prime, other even
;; numbers are not. 
(define (faster-prime? n)
  (cond
   ((= n 2) #t)
   ((even? n) #f)
   (else
    (= n (faster-smallest-divisor n)))))

(define test-primes '(1009 1013 1019 10007 10009 10037 100003 100019 100043
			   1000003 1000033 1000037))

;; The parameter "test" takes a predicate from int->boolean, hopefully either
;; /prime?/ or /faster-prime?/.
(define (abstract-longer-prime-test test n)
  (define (test-kernel n i times)
    (cond
     ((= i times) (test n))
     (else (begin
	     (test n)
	     (test-kernel n (+ i 1) times)))))

  (let ((start-time (runtime)))
    (begin (test-kernel n 0 1000)
           (- (runtime) start-time))))

;; Take a list of integers and run the faster-prime? test on all of them, a
;; thousand times each. Return a list of runtimes.
(define (faster-times primes)
  (map (lambda (prime)
	 (abstract-longer-prime-test faster-prime? prime))
       primes))

;; Take a list of integers and run the prime? test on all of them, a thousand
;; times each. Return a list of runtimes.
(define (slower-times primes)
  (map (lambda (prime) (abstract-longer-prime-test prime? prime))
       primes))

;; Use this like:
;;  (ratios (faster-times test-primes) (slower-times test-primes))
(define (ratios lst1 lst2)
  (map (lambda (x) (* x 1.0)) (map / lst1 lst2)))

;; And here's what we're getting now:
;> (ratios (faster-times test-primes) (slower-times test-primes))
;(0.5263157894736842
; 0.5555555555555556
; 0.5263157894736842
; 0.5084745762711864
; 0.5
; 0.5084745762711864
; 0.49732620320855614
; 0.5053763440860215
; 0.49732620320855614
; 0.5110356536502547
; 0.5264054514480409
; 0.4891846921797005)

;; And the average is:

(define average-list
  (lambda (ls)
    (define sum
      (lambda (ls)
        (if (null? ls)
            0
            (+ (car ls) (sum (cdr ls))))))
    (/ (sum ls) (length ls))))

;>(average-list (ratios (faster-times test-primes) 
;                       (slower-times test-primes)))
;0.5070729743263689

;; Pretty damn close to "twice as fast".  Woohoo!

;;;; 1.24
;; Modify the timed-prime-test procedure of exercise 1.22 to use fast-prime?
;; (the Fermat method), and test each of the 12 primes you found in that
;; exercise.

;; lindseykuper:  Well, I wanted to just take /fast-prime?/ and pass it along
;; to Alex's /abstract-longer-prime-test/ with our list of /test-primes/.  But 
;; /fast-prime?/ takes two arguments.  So first I want to rewrite /fast-prime?/
;; as a unary function so we can use all this accursed scaffolding we had to go
;; and build.  :)

(define (unary-fast-prime? n)
  (fast-prime? n 1)) ;; Let's just use 1 as the number of tests,
                     ;; since the point of this exercise isn't really to test
                     ;; the numbers for primality anyway (we know they're
                     ;; prime!).
  
(define (times-using-fermat-method primes)
  (map (lambda (prime)
	 (abstract-longer-prime-test unary-fast-prime? prime))
       primes))

;; fast-prime? and friends from booksource.

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))        

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

;; Here's what we get:
;> (times-using-fermat-method test-primes)
;(11 12 12 14 14 15 26 27 27 35 34 36)

;; Since the Fermat test has (log n) growth, how would you expect the time to
;; test primes near 1,000,000 to compare with the time needed to test primes
;; near 1000? Do your data bear this out? Can you explain any discrepancy you
;; find?

;; Well, log_2(1,000) =~ 9.966, and log_2(1,000,000) =~ 19.932.  So, I guess
;; it should take twice as long to test primes near 1,000,000 as it does to
;; test primes near 1000.  It's actually taking around three times as long:

;> (ratios (times-using-fermat-method '(1000003 1000033 1000037)) 
;          (times-using-fermat-method '(1009 1013 1019)))
;(3.272727272727273 2.8333333333333335 3.272727272727273)

;; Right now, I'm not really sure why...

;; lindseykuper: actually, here, in case we ever need it.  The built-in /log/
;; is natural log, and log_2(x) = log_2(e) ln(x).
(define log_2
  (lambda (x)
    (* 1.443 (log x))))

;;;; 1.25
;; Alyssa P. Hacker complains that we went to a lot of extra work in writing
;; /expmod/. After all, she says, since we already know how to compute
;; exponentials, we could have simply written

;(define (expmod base exp m)
;  (remainder (fast-expt base exp) m))

;; Is she correct? Would this procedure serve as well for our fast prime
;; tester?  Explain. 

;; lindseykuper: Alyssa's technique is correct and would return the same result
;; as our /expmod/, but the computation would be shaped differently.

;; The /fast-expt/ procedure (which Alyssa's technique uses) takes advantage of
;; the fact that, for even exp, base^exp = (base^(exp/2))^2.  It calculates the
;; two halves separately, then squares them and puts them back together to
;; calculate the final result, base^exp, before finding its remainder mod m.

;; Our /expmod/, on the other hand, never actually calculates base^exp.  It
;; turns out that, just as base^exp = (base^(exp/2))^2,
;; base^exp mod m = ((base^(exp/2) mod m)^2) mod m.  Not having to calculate 
;; base^exp could be a big savings if exp is large.

;; To find out how big of a savings, let's build some test infrastructure, then
;; try using /expmod/ and /alyssa-expmod/ with some big values of exp.  Say,
;; 400,000, 500,000 and 600,000, for instance.

(define 1-25-example-values
  (list 
   '(2 400000 6)
   '(5 500000 10)
   '(8 600000 3)
   ))

;; We'll also want a procedure to turn the above into the combinations we
;; want, e.g., (expmod 2 400000 6) and (alyssa-expmod 2 400000 6).
(define 1-25-example-combinations
  (lambda (list-of-lists proc)
    (map (lambda (ls) (cons proc ls))
         list-of-lists)))

;; And a procedure that will evaluate any combination any number of times and
;; tell us how long it took:

;; combination-running-time: evaluate a combination a certain number of times,
;; display result of the last evaluation, and return total running time.
(define combination-running-time
  (lambda (combination times)
    (define kernel
      (lambda (combination i times start-time)
        (cond
          ((= i times)
           (begin
             (display "result: ")
             (display (eval combination))
             (newline)
             (- (runtime) start-time) ; returned value
             )) 
          (else 
           (begin
             (eval combination)
             (kernel combination (+ i 1) times start-time)
             )))))
    (let ((start-time (runtime)))
      (kernel combination 0 times start-time))))


;; With this, we can do things like:

;> (combination-running-time (square 3) 1)
;result: 9
;0
;> (combination-running-time (square 3) 1000)
;result: 9
;86
;> (combination-running-time (square 300) 1000)
;result: 90000
;88

;; And we can apply it to a list:
(define running-time-each
  (lambda (list-of-combinations times)
    (map (lambda (combination) 
           (combination-running-time combination times)) 
         list-of-combinations)))

;;  Use this like:
; (running-time-each (1-25-example-combinations 1-25-example-values expmod) 10)

;; Cool.  So, here's Alyssa's procedure, with its dependencies:

(define (alyssa-expmod base exp m)
  (remainder (fast-expt base exp) m))

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

;; I find that /expmod/ runs consistently faster that /alyssa-expmod/ on the 
;; values in /1-25-example-values/:

;> (running-time-each 
;   (1-25-example-combinations 1-25-example-values expmod) 10)
;result: 4
;result: 5
;result: 1
;(20 8 10)
;> (running-time-each 
;   (1-25-example-combinations 1-25-example-values alyssa-expmod) 10)
;result: 4
;result: 5
;result: 1
;(33 3448 217)

;; Using /average-list/ and /ratios/, if I'm interpreting this correctly, we
;; see /expmod/ running in about 12% of the time of /alyssa-expmod/ on these
;; data:

;> (average-list
; (ratios
;  (running-time-each 
;   (1-25-example-combinations 1-25-example-values expmod) 10)
;  (running-time-each 
;   (1-25-example-combinations 1-25-example-values alyssa-expmod) 10)))
;result: 4
;result: 5
;result: 1
;result: 4
;result: 5
;result: 1
;0.12168422102327629
         
;; On the other hand, /alyssa-expmod/ can require fewer computation steps for
;; some values of base, exp, and m. Suppose we want to find 2^4 mod 6.

;; Using /alyssa-expmod/:
;(alyssa-expmod 2 4 6)
;(remainder (fast-expt 2 4) 6)
;(remainder (square (fast-expt 2 (/ 4 2))) 6)
;(remainder (square (fast-expt 2 2)) 6)
;(remainder (square (square (fast-expt 2 (/ 2 2)))) 6)
;(remainder (square (square (fast-expt 2 1))) 6)
;(remainder (square (square (* 2 (fast-expt 2 (- 1 1))))) 6)
;(remainder (square (square (* 2 (fast-expt 2 0)))) 6)
;(remainder (square (square (* 2 1))) 6)
;(remainder (square (square 2)) 6)
;(remainder (square 4) 6)
;(remainder 16 6)
;4

;; 13 steps to get an answer.

;; Using /expmod/:
;; Using our own:
;(expmod 2 4 6)
;(remainder (square (expmod 2 (/ 4 2) 6)) 6)
;(remainder (square (expmod 2 2 6)) 6)
;(remainder (square (remainder (square (expmod 2 (/ 2 2) 6)) 6)) 6)
;(remainder (square (remainder (square (expmod 2 1 6)) 6)) 6)
;(remainder (square (remainder (square (remainder 
;                                       (* 2 (expmod 2 (- 1 1) 6)) 6)) 6)) 6)
;(remainder (square (remainder (square (remainder 
;                                       (* 2 (expmod 2 0 6)) 6)) 6)) 6)
;(remainder (square (remainder (square (remainder (* 2 1) 6)) 6)) 6)
;(remainder (square (remainder (square (remainder 2 6)) 6)) 6)
;(remainder (square (remainder (square (remainder 2 6)) 6)) 6)
;(remainder (square (remainder (square 2) 6)) 6)
;(remainder (square (remainder 4 6)) 6)
;(remainder (square 2) 6)
;(remainder 4 6)
;4

;; 15 steps, this way!  These steps don't necessarily take equal time, though.

;; So, in conclusion: it depends.  :)  /expmod/ can be more efficient than 
;; /alyssa-expmod/ for large values of ex.  If we dug deeper, we might be able
;; to find out where the cutoff is.  We also need to take into account how base
;; and m affect the computation, although I'm assuming that exp is more
;; significant, regardless of which version we're running.

;;;; 1.26
;; Louis Reasoner is having great difficulty doing exercise 1.24.  His
;; /fast-prime?/ test seems to run more slowly than his /prime?/ test.  Louis 
;; calls his friend Eva Lu Ator over to help.  When they examine Louis's code,
;; they find that he has written the /expmod/ procedure to use an explicit 
;; multiplication, rather than calling /square/:

;(define (expmod base exp m)
;  (cond ((= exp 0) 1)
;        ((even? exp)
;         (remainder (* (expmod base (/ exp 2) m)
;                       (expmod base (/ exp 2) m))
;                    m))
;        (else
;         (remainder (* base (expmod base (- exp 1) m))
;                    m))))

;; "I don't see what difference that could make," says Louis.  "I do," says
;; Eva. "By writing the procedure like that, you have transformed the 0(log n)
;; process into a 0(n) process."  Explain.

;; lindseykuper: The expression

; (* (expmod base (/ exp 2) m) (expmod base (/ exp 2) m))

;; will take longer to evaluate than the expression 

; (square (expmod base (/ exp 2) m))

;; because of applicative-order evaluation.  The /*/ and /square/ procedures
;; evaluate their arguments before applying, so in the first case,
;; (expmod base (/ exp 2) m) gets calculated twice.  This is especically
;; inefficient because it's a recursive call to /expmod/, so our procedure
;; generates two identical recursive processes!

;; So, we know Louis' procedure is slower.  But why is it specifically O(n) 
;; now, instead of O(log n)?  Well, we're still dividing exp by 2 in every
;; step, so it still takes (log n) steps to get to the bottom of the recursion
;; tree.  But with Louis' version, the number of times we have to perform the
;; same computation increases by powers of 2 with each step down the tree!  
;; So, the (log n) and the (n^2) "cancel out", in a sense, leaving us with 
;; O(n) for the whole thing.  Ouch.

;;;; 1.27
;; Demonstrate that the Carmichael numbers listed in footnote 47 really do fool
;; the Fermat test.  That is, write a procedure that takes an integer /n/ and
;; tests whether /a^n/ is congruent to /a/ modulo /n/ for every /a/ < /n/, and
;; try your procedure on the given Carmichael numbers.

;; lindseykuper:

(define carmichael-numbers '(561 1105 1729 2465 2821 6601))

(define congruent?
  (lambda (x y n)
    (= (remainder x n) (remainder y n))))

(define fools-fermat?
  (lambda (n)
    (fools-fermat-kernel n 1)))

(define fools-fermat-kernel
  (lambda (n a)
    (cond ((= n a) #t)
          ((not (congruent? (fast-expt a n) (remainder a n) n)) #f)
          (else (fools-fermat-kernel n (+ a 1))))))

(define test-carmichael-numbers
  (lambda ()
    (map fools-fermat? carmichael-numbers)))

; > (test-carmichael-numbers)
;(#t #t #t #t #t #t)

;; And also, just for the heck of it, here's a procedure for finding Carmichael
;; numbers (although it's terribly slow and I wouldn't want to use it to find
;; any larger than the ones we already know).

(define find-carmichael-numbers-below
  (lambda (test-up-to)
    (letrec ((kernel
              (lambda (i)
                (cond ((= i test-up-to) (display "Done!"))
                      ((and (not (prime? i)) (fools-fermat? i)) 
                       (begin
                         (display i)
                         (newline)
                         (kernel (+ i 1))))
                      (else (kernel (+ i 1)))))))
      (kernel 1))))

;;;; 1.28
;; ...Modify the expmod procedure to signal if it discovers a nontrivial
;; square root of 1, and use this to implement the Miller-Rabin test with a 
;; procedure analogous to fermat-test. Check your procedure by testing various 
;; known primes and non-primes. Hint: One convenient way to make expmod signal 
;; is to have it return 0.

;; lindseykuper:

;; "congruent mod n" means "both have the same remainder when divided by n".
;; j and k are congruent mod n iff (j mod n) = (k mod n).

;; Fermat's Little Theorem ("alternate" form):
;; If n is prime and a is a pos. int. < n,
;; a^(n - 1) mod n = 1 mod n.
;; 1 mod n is always just n for positive integers, so:
;; a^(n - 1) mod n = 1.

;; I'd like to find a more elegant way of implementing this so I'm not calling
;; /square/ twice and /expmod/ twice, but I can't seem to think of one.
(define (miller-rabin-expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)  
         (if (nontrivial-sqrt? (expmod base (/ exp 2) m) m)
             0
             (remainder (square (expmod base (/ exp 2) m)) m)))
        (else
         (remainder (* base (expmod base (- exp 1) m)) m))))

(define nontrivial-sqrt?
  (lambda (num n)
    ; a number not equal to 1 or n - 1 whose square is equal to 1 modulo n.
    (and (not (= num 1)) 
         (not (= num (- n 1))) 
         (= (square num) (remainder 1 n)))))

(define (miller-rabin-test n)
  (define (try-it a)
    ;; miller-rabin-expmod returns 0 if there exists a 
    ;; "nontrivial sqrt of 1 mod n", which would mean
    ;; that n isn't prime.
    (= (remainder 1 n) (miller-rabin-expmod a (- n 1) n)))
  (try-it (+ 1 (random (- n 1)))))

(define (miller-rabin-prime? n times)
  (cond ((= times 0) true)
        ((miller-rabin-test n) (miller-rabin-prime? n (- times 1)))
        (else false)))

(define (unary-miller-rabin-prime? n)
  (miller-rabin-prime? n 10))

;; We might need these.
(define test-composites
  '(4 6 8 9 10 12 14 15 16 18 20 21 22 24 25 26 27 28 30 32 33 34 35 36 38 39
      40 42 44 45 46 48 49 50 51 52 54 55 56 57 58 60 62 63 64 65 66 68 69 70
      72 74 75 76 77 78 80 81 82 84 85 86 87 88 90 91 92 93 94 95 96 98 99 100
      102 104 105 106 108 110 111 112 114 115 116 117 118 119 120))

;; Might need this, too.  Takes two lists of equal length.
(define pair-lists
  (lambda (list1 list2)
    (if (null? list1)
        '()
        (cons (list (car list1) (car list2)) 
              (pair-lists (cdr list1) (cdr list2))))))

;; When run like this:

;(pair-lists test-composites (map miller-rabin-test
;                                 test-composites))

;; I usually get a few false positives for a few numbers in /test-composites/.
;; But if I do the test 10 times:

;(pair-lists test-composites (map unary-miller-rabin-prime?
;                                 test-composites))

;; then I get #f for everything.  I'm also getting:

;> (pair-lists carmichael-numbers (map unary-miller-rabin-prime?
;                                      carmichael-numbers))
;((561 #f) (1105 #f) (1729 #f) (2465 #f) (2821 #f) (6601 #f))

;; Yay!  Also, this might be useful sometime:

(define generate-sequence
  (lambda (start finish)
    (if (= start (+ 1 finish))
        ()
        (cons start (generate-sequence (+ start 1) finish)))))

;;;; 1.29
;; ...Using Simpson's Rule, the integral of a function /f/ between /a/ and /b/
;; is approximated as

; h
; - [ y_0 + 4y_1 + 2y_2 + 4y_3 + 2y_4 + ... + 2y_(n-2) + 4y_(n-1) + y_n ] 
; 3

;; where h = (b - a)/n, for some even integer /n/, and y_k = f(a + kh).

;; ...Define a procedure that takes as arguments /f/, /a/, /b/ and /n/ and 
;; returns the value of the integral, computed using Simpson's Rule.  Use your
;; procedure to integrate /cube/ between 0 and 1 (with /n/ = 100 and /n/ =
;; 1000), and compare the results to those of the /integral/ procedure shown
;; above.

;; lindseykuper:

(define simpsons-rule
  (lambda (f a b n)
    (letrec ((h (/ (- b a) n))
             (y (lambda (k)
                  (f (+ a (* k h)))))
             
             ;; n is even, so, n-1 is odd.
             ;; coefficient is always 4 for odd k, and always 2 for even k.
             (next-coefficient (lambda (k)
                                 (if (even? k) 
                                     2
                                     4)))
             (finalize (lambda (f a n summation)
                         (* (/ h 3) 
                            (+ summation
                               ;; the special terms are y_0 and y_n,
                               ;; which don't have a coefficient at all.
                               (f a)                 ; y_0
                               (f (+ a (* n h))))))) ; y_n
             
             (kernel (lambda (f a b n k accumulator)
                       (if (= k n)
                           (finalize f a n accumulator)
                           (kernel f a b n (+ k 1) 
                                      (+ accumulator 
                                         (* (next-coefficient k) 
                                            (y k))))))))
      (kernel f a b n 0 0))))

(define (cube x) (* x x x))

;; (simpsons-rule cube 0 1 100) and (simpsons-rule cube 0 1 1000) both return
;; 1/4, which is the exact value of the integral.  Hell yeah!

;;;; 1.30

;(define (sum term a next b)
;  (if (> a b)
;      0
;      (+ (term a)
;         (sum term (next a) next b))))

;; The /sum/ procedure above generates a linear recursion.  The procedure can
;; be rewritten so that the sum is performed iteratively.  Show how to do this
;; by filling in the missing expressions in the following definition:

;(define (sum term a next b)
;  (define (iter a result)
;    (if <??>
;        <??>
;        (iter <??> <??>)))
;  (iter <??? <??>))

;; lindseykuper:
;; Well, /a/ is the only parameter that changes in the repeated calls to /sum/.
;; It gets replaced with the value of /(next a)/.  But /term/, /next/, and /b/ 
;; all stay the same.

;; I'm betting that /result/ is what we want to return at the end.  We can also
;; use it as an accumulator and have it start at 0.

;; Let's call this "summation" -- less confusing.

(define (summation-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ (term a) result))))
  (iter a 0))

;; seems to work!

;;;; 1.31a 
;; ...Write an analogous procedure called /product/ that returns the
;; product of the values of a function at points over a given range.
;; Show how to define /factorial/ in terms of /product/.  Also use
;; /product/ to compute approximations to pi using the formula
;;
;; pi   2 * 4 * 4 * 6 * 6 * 8 ...
;; -- = -------------------------
;;  4   3 * 3 * 5 * 5 * 7 * 7 ...

;; lindseykuper:

(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

(define (factorial num) (product * 1 inc num))

(define approximation-to-pi
  (lambda (accuracy)
    (define next
      (lambda (num)
        (+ 2 num)))
    (define term
      (lambda (num)
        (if (= num 2)
            (/ 2 3)
            (/ (* num num) (* (- num 1) (+ num 1))))))
    (exact->inexact (* 4 (product term 2 next accuracy)))))
           
;;;; 1.31b
;; ...write one that generates an iterative process.

(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* (term a) result))))
  (iter a 1))

;;;; 1.32a
;; ...Write /accumulate/ and show how /sum/ and /product/ can both be defined
;; as simple calls to /accumulate/.

(define accumulate
  (lambda (combiner null-value term a next b)
    (if (> a b)
        null-value
        (combiner (term a)
                  (accumulate combiner null-value term (next a) next b)))))

(define sum-using-accumulate
  (lambda (term a next b)
    (accumulate + 0 term a next b)))

(define product-using-accumulate
  (lambda (term a next b)
    (accumulate * 1 term a next b)))

;;;; 1.32b
;; ...write one that generates an iterative process.

(define accumulate-iter
  (lambda (combiner null-value term a next b)
    (define (iter a result)
      (if (> a b)
        result
        (iter (next a) (combiner (term a) result))))
    (iter a null-value)))

;;; 1.33
;; ...Write /filtered-accumulate/ as a procedure. 

(define filtered-accumulate
  (lambda (pred combiner null-value term a next b)
    (if (> a b)
        null-value
        (if (pred a)
            (combiner (term a)
                      (filtered-accumulate 
                       pred combiner null-value term (next a) next b))
            (filtered-accumulate
             pred combiner null-value term (next a) next b)))))

;; And also!:

(define filtered-accumulate-iter
  (lambda (pred combiner null-value term a next b)
    (define (iter a result)
      (if (> a b)
        result
        (if (pred a)
            (iter (next a) (combiner (term a) result))
            (iter (next a) result))))
    (iter a null-value)))

;; Show how to express the following using /filtered-accumulate/: 

;; a. the sum of the squares of the prime numbers in the interval /a/ to /b/
;; (assuming that you have a /prime?/ predicate already written)

(define sum-of-squares-of-primes
  (lambda (a b)
    (filtered-accumulate-iter prime? + 0 square a inc b)))

;; b. the product of all the positive integers less than /n/ that are
;; relatively prime to /n/ (i.e., all positive integers /i/ < /n/ such that
;; GCD(/i/, /n/) = 1).

(define product-of-relative-primes
  (lambda (n)
    (letrec ((self (lambda (n) n))
             (relatively-prime-to-n? (lambda (i)
                                       (= 1 (gcd i n))))
             (gcd (lambda (a b) ;; Euclid's Algorithm from p. 49 of SICP
                    (if (= b 0)
                        a
                        (gcd b (remainder a b))))))
      (filtered-accumulate-iter relatively-prime-to-n? * 1 self 1 inc n))))
    
;;;; 1.34
;; Suppose we define the procedure

(define (f g)
  (g 2))

;; Then we have
;;
;: > (f square)
;; 4
;;
;: > (f (lambda (z) (* z (+ z 1))))
;; 6
;;
;; What happens if we (perversely) ask the interpreter to evaluate the
;; combination /(f f)/?  Explain.

;; lindseykuper:
;; /f/ takes a function and returns the value of that function called with 2
;; as its argument.  So,

;; (f f)

;; will evaluate to

;; (f 2)

;; which will evaluate to

;; (2 2) 

;; which will fail, because 2 isn't a procedure.

;; (Incidentally, /f/ will only work if the function we pass to it has arity 1.
;; So, (f fib) would return 1, but (f xor) would return an error because the
;; /xor/ procedure expects 2 arguments.)

;;;; 1.35
;; Show that the golden ratio phi is a fixed point of the transformation
;; x --> 1 + 1/x...

;; A fixed point of x --> 1 + 1/x is a point at which 
;; x = 1 + 1/x, which is equivalent to x^2 = x + 1.  The golden ratio is the
;; number phi which satisfies the equation phi^2 = phi + 1.  Therefore a fixed
;; point of x --> 1 + 1/x is phi, the golden ratio.

;; ...and use this fact to compute phi by means of the /fixed-point/ procedure.

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define golden-ratio-using-fixed-point
  (fixed-point (lambda (x) (+ 1 (/ 1 x)))
               1.0))

;> golden-ratio-using-fixed-point
;1.6180327868852458

;;;; 1.36

(define (verbose-fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (begin
        (display guess)
        (newline)
        (if (close-enough? guess next)
            next
            (try next)))))
  (try first-guess))

(define exercise-1-36-without-average-damping
  (lambda ()
    (verbose-fixed-point (lambda (x) (/ (log 1000) (log x)))
                 4.0)))

;> (exercise-1-36-without-average-damping)
;4.0
;4.9828921423310435
;4.301189432497896
;4.734933901055578
;4.442378437719526
;4.632377941509958
;4.505830646780212
;4.588735606875766
;4.533824356566501
;4.56993352418142
;4.546075272637246
;4.561789745175654
;4.55141783665413
;4.5582542120702625
;4.553744140202578
;4.556717747893265
;4.554756404545319
;4.5560497413912975
;4.5551967522618035
;4.555759257615811
;4.555388284933278
;4.555632929754932
;4.555471588998784
;4.555577989320218
;4.555507819903776
;4.555554095154945
;4.555523577416557
;4.555543703263474
;4.555530430629037
;4.555539183677709

;; 30 steps without average damping.

;; For the version with average damping, we take the original function, add /x/
;; to both sides and divide by 2.

;;  x -->           log(1000)/log(x)
;; 2x -->       x + log(1000)/log(x)
;;  x --> (1/2)(x + log(1000)/log(x))

(define exercise-1-36-with-average-damping
  (lambda ()
    (verbose-fixed-point (lambda (x) (average x (/ (log 1000) (log x))))
                 4.0)))

;> (exercise-1-36-with-average-damping)
;4.0
;4.491446071165521
;4.544974650975552
;4.553746974742814
;4.555231425802502
;4.555483906560562
;4.5555268862194875
;4.5555342036887705

;; Only 8 steps.  (That is, 8 times through /fixed-point/, but not necessarily
;; less processing.)

;;;; 1.37a
;; ...Define a procedure /cont-frac/ such that evaluating /(cont-frac n d k)/
;; computes the value of the /k/-term finite continued fraction.  Check your
;; procedure by approximating 1/phi using

; (cont-frac (lambda (i) 1.0)
;            (lambda (i) 1.0)
;            k)

;; for successive values of /k/.  How large must you make /k/ in order to get
;; an approximation that is accurate to 4 decimal places?

(define cont-frac
  (lambda (n d k)
    (define kernel
      (lambda (i)
        (if (= i k)
            0
            (/ (n i) (+ (d i) (kernel (+ i 1)))))))
    (kernel 1)))

;; If we're looking for 0.6180, we have to use /k/ = 12:

; > (cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 12)
; 0.6180555555555556

;;;; 1.37b
;; ...write one that generates an iterative process.

;;;; 1.38
;; ...Write a program that uses your /cont-frac/ procedure from exercise 1.37
;; to approximate /e/, based on Euler's expansion.

(define approximate-e
  (lambda (accuracy)
    (letrec ((n (lambda (i) 1.0))
             (d (lambda (i)
                  ;; For the /i/th term in the sequence, if (i + 1) mod 3 = 0,
                  ;; this is one of the "special" terms.  Otherwise, it's 1.
                  (if (= 0 (remainder (+ i 1) 3))
                      (- i (subtrahend i))
                      1)))
             (subtrahend (lambda (i)
                           ;; Assumes that its argument satisfies the equation
                           ;; (i + 1) mod 3 = 0
                           (if (= i 2)
                               0
                               (+ 1 (subtrahend (- i 3)))))))
      (+ 2 (cont-frac n d accuracy)))))

; > (approximate-e 100)
; 2.7182818284590455

;; Woot!

;;;; 1.39
;; ...Define a procedure /(tan-cf x k)/ that computes an approximation to the 
;; tangent function based on Lambert's formula.

(define tan-cf
  (lambda (x k)
    (let ((n (lambda (i)
               (if (= i 1)
                   x
                   (- (square x)))))
          (d (lambda (i)
               (if (= i 1)
                   1
                   (- (* i 2) 1)))))
      (exact->inexact (cont-frac n d k)))))

;;;; 1.40
;; Define a procedure /cubic/ that can be used together with the
;; /newtons-method/ procedure in expressions of the form

; (newtons-method (cubic a b c) 1)

;; to approximate zeros of the cubic x^3 + ax^2 + bx + c.

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))
(define dx 0.00001)

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define cubic
  (lambda (a b c)
    ;; return another procedure which has one argument
    (lambda (x)
      (+ (cube x) (* a (square x)) (* b x) c))))

;; If it works, then these should all come out close to 0...

;> ((cubic 57 900000 12) (newtons-method (cubic 57 900000 12) 1))
;1.7763568394002505e-15
;> ((cubic 2 1 5) (newtons-method (cubic 2 1 5) 1))
;2.0581758519711002e-11
;> ((cubic 16 9 3) (newtons-method (cubic 16 9 3) 1))
;1.1851852832478471e-11

;; They look pretty good to me.

;;;; 1.41
;; Define a procedure /double/ that takes a procedure of one argument as 
;; argument and returns a procedure that applies the original procedure twice.

(define double
  (lambda (f)
    (lambda (x)
      (f (f x)))))

; > (((double (double double)) inc) 5)
; 21

;;;; 1.42
;; ...Define a procedure /compose/ that implements composition.

(define compose
  (lambda (f g)
    (lambda (x)
      (f (g x)))))

;;;; 1.43
;; ...Write a procedure that takes as inputs a procedure that computes /f/ and
;; a positive integer /n/ and returns the procedure that computes the /n/th 
;; repeated application of /f/.

(define repeated
  (lambda (f n)
    (if (= n 1)
        (lambda (x) (f x))
        (compose f (repeated f (- n 1))))))

;;;; 1.44
;; ...Write a procedure /smooth/ that takes as input a procedure that computes
;; /f/ and returns a procedure that computes the smoothed /f/.

(define smooth 
  (lambda (f)
    (lambda (x)
      (/ (+ (f (- x dx)) (f x) (f (+ x dx))) 3))))
      
;; ...Show how to generate the /n/-fold smoothed function of any given function
;; using /smooth/ and /repeated/ from exercise 1.43.

;; You can do it like:

; > (((repeated smooth 2) square) 2)
; 4.000000000133333
; > (((repeated smooth 3) square) 2)
; 4.0000000002
; > (((repeated smooth 4) square) 2)
; 4.000000000266667

;; which will turn out the same as

; > (((compose smooth smooth) square) 2)
; 4.000000000133333
; > (((compose (compose smooth smooth) smooth) square) 2)
; 4.0000000002
; > (((compose (compose (compose smooth smooth) smooth) smooth) square) 2)
; 4.000000000266667

;; , respectively.  We could also generalize it into a function:

(define n-fold-smoothed-function
  (lambda (f n)
    (lambda (x)
      (((repeated smooth n) f) x))))

;; which lets us do:

; > ((n-fold-smoothed-function square 2) 2)
; 4.000000000133333
; > ((n-fold-smoothed-function square 3) 2)
; 4.0000000002
; > ((n-fold-smoothed-function square 4) 2)
; 4.000000000266667

;;;; 1.45
;; ...Do some experiments to determine how many average damps are required...

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (cube-root x)
  (fixed-point (average-damp (lambda (y) (/ x (square y))))
               1.0))

(define (fourth-root x)
  (fixed-point ((repeated average-damp 2) (lambda (y) (/ x (expt y 3))))
               1.0))

(define (fifth-root x)
  (fixed-point ((repeated average-damp 2) (lambda (y) 
                                            (/ x (expt y 4))))
               1.0))

(define (sixth-root x)
  (fixed-point ((repeated average-damp 2) (lambda (y) 
                                            (/ x (expt y 5))))
               1.0))

;; It looks to me like average-damping twice is enough for fourth, fifth, and
;; sixth roots.  For instance, for the above, I get:

; > (fourth-root (expt 500 4))
; 500.0
; > (fifth-root (expt 500 5))
; 500.0000007238933
; > (sixth-root (expt 500 6))
; 499.9999975242963

;; Let's go ahead and write the /nth-root/ procedure, but with a parameter for
;; the number of average damps...

(define (nth-root x n damps)
  (fixed-point ((repeated average-damp damps) (lambda (y)
                                           (/ x (expt y (- n 1)))))
               1.0))

;; /(nth-root (expt 5 20) 20 2)/ and /(nth-root (expt 5 20) 20 3)/ don't
;; converge, but:

; > (nth-root (expt 5 20) 20 4)
; 4.999998544071863

;; So, 20th roots need to be damped 4 times -- interesting!  Where's the
;; cutoff?

; > (nth-root (expt 5 7) 7 2)
; 5.000004267274231
; > (nth-root (expt 5 8) 8 2)  ;; 8th root needs to be damped 3 times!
; user break

; ...

; > (nth-root (expt 5 15) 15 3)
; 4.999995762170965
; > (nth-root (expt 5 16) 16 3) ;; 16th root needs to be damped 4 times!
; user break

;; And it looks like 32nd root needs to be damped 5 times, etc.

;; So, for 4  <= n <= 7, twice.
;;     for 8  <= n <= 15, three times.
;;     for 16 <= n <= 31, four times.
;;     for 32 ...

;; It looks like the number of damps needed to compute the nth root is
;; floor(log n).  Cool!  So we can redefine /nth-root/ this way (using our own
;; /log_2/ from above):

(define (nth-root x n)
  (fixed-point ((repeated average-damp (floor (log_2 n))) 
                (lambda (y) (/ x (expt y (- n 1)))))
               1.0))

; > (nth-root (expt 4999 34) 34)
; 4999.00000009471

;;;; 1.46
;; ...Write a procedure /iterative-improve/ that takes two procedures as
;; arguments: a method for telling whether a guess is good enough and a method
;; for improving a guess.  /Iterative-improve/ should return as its value a
;; procedure that takes a guess as argument and keeps improving the guess
;; until it is good enough. Rewrite the /sqrt/ procedure of section 1.1.7 and
;; the /fixed-point/ procedure of section 1.3.3 in terms of
;; /iterative-improve/.

(define iterative-improve
  (lambda (good-enough? improve)
    (letrec ((improver (lambda (guess)
                         (if (good-enough? guess)
                             guess
                             (improver (improve guess))))))
      (lambda (guess)
        (improver guess)))))

(define sqrt-using-iterative-improve
  (lambda (x)
    (define (improve guess)
      (average guess (/ x guess)))
    (define (good-enough? guess)
      (< (abs (- (square guess) x)) 0.001))
    ((iterative-improve good-enough? improve) 1.0)))

(define fixed-point-using-iterative-improve
  (lambda (f first-guess)
    (define (next guess)
      (f guess))
    (define (close-enough? guess)
      (< (abs (- guess (next guess))) tolerance))
    ((iterative-improve close-enough? next) first-guess)))

    

