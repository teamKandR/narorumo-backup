;; Structure and Interpretation of Computer Programs Exercises
;; Chapter 1
;; Lindsey Kuper and Alex Rudnick

;;;; 1.1
;; What is the result printed by the interpreter in response to each
;; expression? ...


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
;; Observe that our model of evaluation allows for combinations whose operators
;; are compound expressions. Use this observation to describe the behavior of
;; the following procedure:
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

;; The body of the function takes the form:
;; ( (expression that evaluates to a function) a b)
;; ... and the value of the expression is either + or -, such that we always add
;; the absolute value of b to a.

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
;; Haskell? Or is it like thunks and call-by-name? Or are these the same thing?
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
;; large numbers. An alternative strategy for implementing /good-enough?/ is to
;; watch how /guess/ changes from one iteration to the next and to stop when te
;; change is a very small faction of the guess. Design a square-root procedure
;; that uses this kind of test. Does this work better for small and large
;; numbers?
(define (sqrt-iter guess x)
  (print guess)
  (newline)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(define (sqrt x)
  (sqrt-iter 1.0 x))

;; alexr: Well, in the case of small numbers, once we're working at a small
;; enough order of magnitude, all numbers will be marked as Good Enough. For
;; example, the square root of 1e-20 is really 1e-10. But 0.01 (eg, 1e-2) is
;; marked by /good-enough?/ as an acceptable approximation of its sqrt -- at 8
;; orders of magnitude off. (sqrt 1e-20) is calculated as 0.03125, with our
;; current code.

;; alexr: For very large numbers, the problem deals with the floating-point
;; representation of numbers in the computer -- they aren't
;; arbitrary-precision. Particularly, in the case of trying to compute the sqrt
;; of 3e20, the procedure (on mzscheme on amd64, anyway) quickly converges
;; 17320508075.688774, which is pretty close to the sqrt of 3e20, but not
;; good-enough, as such. Importantly, the improve function doesn't move it at
;; all; there are only so many bits to represent numbers any closer to the
;; actual sqrt. So in this case, the call never terminates...

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
;; approximation to the cube root of x,  then a better approximation is given by
;; the value (/ (+ (/ x (expt y 2)) (* 2 y)) 3).
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
;; Each of the following two procedures defines a method for adding two positive
;; integers in terms of the procedures inc and dec.

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
;; procedure in evaluating (+ 4 5). Are these processes iterative or recursive?

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

;; alexr: (f n) doubles the input. Since f passes 0 as the first argument to A,
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
;; no, because h(2) = 4, not 16.  We need something that doesn't expand *quite* so fast.

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

;; lindseykuper and alexr: (h n) is the expansion of n (g (g ... 2)) (for n nested instances
;; of /g/) -- which is to say that it's 2^(2^(2 ...)). It's 2 with a bunch of exponents going
;; up diagonally, of length n. Win!

;;;; 1.11
;; A function f is defined by the rule that f(n) = n if n<3 and f(n) = f(n - 1)
;; + 2f(n - 2) + 3f(n - 3) if n >= 3. Write a procedure that computes f by means
;; of a recursive process. Write a procedure that computes f by means of an
;; iterative process.

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
;; ... Write a procedure that computes elements of Pascal's triangle by means of a
;; recursive process. 

;;       1
;;      1 1
;;     1 2 1
;;    1 3 3 1
;;   1 4 6 4 1
;;  1 5 A A 5 1

;; Yoshi! ("Do it!", roughly)

;; alexr: Base case: if it's "on the edge", it's a 1.
;; The ith row is i+1 elements wide. So an element is On The Edge if its index is 0 or i.

;; lindseykuper: So, to get (row 3, col 1), which is supposed to be 3,
;; check if col == 0 or 3.  It's not.
;; So, then, look at row "row-1", cols "col-1" and "col".  The sum of those is the answer.

;; The most that row can ever be is col-1.

(define pascals-triangle
  ;"Returns the value of one element in Pascal's Triangle at specified coordinates."
  (lambda (row col)
    (cond
      ((or (= col 0) (= col row)) 1)
      ((> col row) 'dontdothat) 
      (else 
       (+ (pascals-triangle (- row 1) (- col 1))
          (pascals-triangle (- row 1) col))))))
