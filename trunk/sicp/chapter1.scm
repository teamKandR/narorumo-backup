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

;; lindseykuper:
;; always expand (or contract) only the last term.
;(count-change 11)
;(cc 11 5)
;(+ (cc 11 4) (cc -39 5))                                      ; 11 - 50 = -39
;(+ (cc 11 4) 0)
;(+ (+ (cc 11 3) (cc -14 4)) 0)                                ; 11 - 25 = -14
;(+ (+ (cc 11 3) 0) 0) 
;(+ (+ (+ (cc 11 2) (cc 1 3)) 0) 0)                            ; 11 - 10 =   1
;(+ (+ (+ (cc 11 2) (+ (cc 1 2) (cc -9 3))) 0) 0)              ;  1 - 10 = - 9
;(+ (+ (+ (cc 11 2) (+ (cc 1 2) 0)) 0) 0)
;(+ (+ (+ (cc 11 2) (+ (+ (cc 1 1) (cc 0 2)) 0)) 0) 0)
;(+ (+ (+ (cc 11 2) (+ (+ (cc 1 1) 1) 0)) 0) 0)
;(+ (+ (+ (cc 11 2) (+ (+ (cc 1 1) 1) 0)) 0) 0)
;(+ (+ (+ (cc 11 2) (+ (+ (+ (cc 1 0) (cc 0 1)) 1) 0)) 0) 0)
;(+ (+ (+ (cc 11 2) (+ (+ (+ (cc 1 0) 1) 1) 0)) 0) 0)
;(+ (+ (+ (cc 11 2) (+ (+ (+ 0 1) 1) 0)) 0) 0)
;(+ (+ (+ (cc 11 2) (+ (+ 1 1) 0)) 0) 0)
;(+ (+ (+ (cc 11 2) (+ 2 0)) 0) 0)
;(+ (+ (+ (cc 11 2) 2) 0) 0)
;(+ (+ (+ (+ (cc 11 1) (cc 6 2)) 2) 0) 0)                      ; 11 -  5 =   6
;(+ (+ (+ (+ (cc 11 1) (+ (cc 6 1) (cc 1 2)) ) 2) 0) 0)        ;  6 -  5 =   1
;(+ (+ (+ (+ (cc 11 1) (+ (cc 6 1) (+ (cc 1 1) 
;                                     (cc 0 2))) ) 2) 0) 0)    ;  1 -  1 =   0
;(+ (+ (+ (+ (cc 11 1) (+ (cc 6 1) (+ (cc 1 1) 
;                                     1)) ) 2) 0) 0)
;(+ (+ (+ (+ (cc 11 1) (+ (cc 6 1) (+ (+ (cc 1 0) (cc 0 1)) 1)) ) 2) 0) 0)
;(+ (+ (+ (+ (cc 11 1) (+ (cc 6 1) (+ (+ (cc 1 0) 0) 1)) ) 2) 0) 0)
;(+ (+ (+ (+ (cc 11 1) (+ (cc 6 1) (+ (+ 0 0) 1)) ) 2) 0) 0)
;(+ (+ (+ (+ (cc 11 1) (+ (cc 6 1) (+ 0 1)) ) 2) 0) 0)
;(+ (+ (+ (+ (cc 11 1) (+ (+ (cc 6 0) (cc 5 1)) 1) ) 2) 0) 0)
;(+ (+ (+ (+ (cc 11 1) (+ (+ (cc 6 0) (+ (cc 5 0) (cc 4 1))) 1) ) 2) 0) 0)
;(+ (+ (+ (+ (cc 11 1) (+ (+ (cc 6 0) 
;	(+ (cc 5 0) (+ (cc 4 0) (cc 3 1)))) 1) ) 2) 0) 0)
;(+ (+ (+ (+ (cc 11 1) (+ (+ (cc 6 0) 
;	(+ (cc 5 0) (+ (cc 4 0) (+ (cc 3 0) (cc 2 1))))) 1) ) 2) 0) 0)
;(+ (+ (+ (+ (cc 11 1) (+ (+ (cc 6 0) 
;	(+ (cc 5 0) (+ (cc 4 0) (+ (cc 3 0) 
;		(+ (cc 2 0) (cc 1 1)))))) 1) ) 2) 0) 0)
;(+ (+ (+ (+ (cc 11 1) (+ (+ (cc 6 0) 
;	(+ (cc 5 0) (+ (cc 4 0) (+ (cc 3 0) 
;		(+ (cc 2 0) (+ (cc 1 0) (cc 0 1))))))) 1) ) 2) 0) 0)
;(+ (+ (+ (+ (cc 11 1) (+ (+ (cc 6 0) 
;	(+ (cc 5 0) (+ (cc 4 0) (+ (cc 3 0) 
;		(+ (cc 2 0) (+ (cc 1 0) 1)))))) 1) ) 2) 0) 0)
;(+ (+ (+ (+ (cc 11 1) (+ (+ (cc 6 0) 
;	(+ (cc 5 0) (+ (cc 4 0) (+ (cc 3 0) (+ (cc 2 0) (+ 0 1)))))) 1) ) 2) 0) 0)
;(+ (+ (+ (+ (cc 11 1) (+ (+ (cc 6 0) 
;	(+ (cc 5 0) (+ (cc 4 0) (+ (cc 3 0) (+ (cc 2 0) 1))))) 1) ) 2) 0) 0)
;(+ (+ (+ (+ (cc 11 1) (+ (+ (cc 6 0) 
;	(+ (cc 5 0) (+ (cc 4 0) (+ (cc 3 0) (+ 0 1))))) 1) ) 2) 0) 0)
;(+ (+ (+ (+ (cc 11 1) (+ (+ (cc 6 0) 
;	(+ (cc 5 0) (+ (cc 4 0) (+ (cc 3 0) 1)))) 1) ) 2) 0) 0)
;(+ (+ (+ (+ (cc 11 1) (+ (+ (cc 6 0) 
;	(+ (cc 5 0) (+ (cc 4 0) (+ 0 1)))) 1) ) 2) 0) 0)
;(+ (+ (+ (+ (cc 11 1) (+ (+ (cc 6 0) 
;	(+ (cc 5 0) (+ (cc 4 0) 1))) 1) ) 2) 0) 0)
;(+ (+ (+ (+ (cc 11 1) (+ (+ (cc 6 0) (+ (cc 5 0) (+ 0 1))) 1) ) 2) 0) 0)
;(+ (+ (+ (+ (cc 11 1) (+ (+ (cc 6 0) (+ (cc 5 0) 1)) 1) ) 2) 0) 0)
;(+ (+ (+ (+ (cc 11 1) (+ (+ (cc 6 0) (+ 0 1)) 1) ) 2) 0) 0)
;(+ (+ (+ (+ (cc 11 1) (+ (+ (cc 6 0) 1) 1) ) 2) 0) 0)
;(+ (+ (+ (+ (cc 11 1) (+ (+ 0 1) 1) ) 2) 0) 0)
;(+ (+ (+ (+ (cc 11 1) (+ 1 1) ) 2) 0) 0)
;(+ (+ (+ (+ (cc 11 1) 2 ) 2) 0) 0)
; ...grr.  This is going to work out to 5, and that ain't right.  Where's my 
; mistake?

; 4 ways:
; 11 pennies
; 2 nickels, 1 penny
; 1 nickel, 6 pennies
; 1 dime, 1 penny

; I used one shortcut: there's only one way to change amounts less than 5, 
; because:
;
;(cc 4 1)
;(+ (cc 4 0) (cc 3 1))
;(+ 0 (cc 3 1))
;
;(cc 3 1)
;(+ (cc 3 0) (cc 2 1))
;(+ 0 (cc 2 1))
;
;(cc 2 1)
;(+ (cc 2 0) (cc 1 1))
;(+ 0 (cc 1 1))
;
;(cc 1 1)
;(+ (cc 1 0) (cc 0 1))
;(+ 0 1)
;1

;;; I had to compute (cc 1 2) twice, maybe three times.

;;; optimization idea: test first to see whether /amount/ is less than any of 
;;; the denominations, and only use denominations that are less then or equal 
;;; to /amount/.

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

;; lindseykuper: No, I take it back!  The trouble with doing a = ab^(n/2) is that,
;; in every step, we have to calculate another exponent to a power that we don't 
;; know yet.  So this can never be entirely iterative, because we'll keep on
;; stacking up recursive calls to fast-expt-iter so that we can find b^(n/2).

;; However!  
;; We know that: b^n = b^(n/2) * b^(n/2) = (b^(n/2))^2 = (b^2)^(n/2).

;; So, (fast-expt-iter b n) ought to be the same as 
;; (fast-expt-iter (square b) (/ n 2)).

;; Why can't we just do this?

;; (define fast-expt-iter
;;   (lambda (b n)
;;   (fast-expt-iter (square b) (/ n 2))))

;; Well, that would be great if  /n/ were always even (i.e., if it were a power
;; of 2 initially).  But we can't guarantee that.  So we need the state variable
;; /a/ *just* to handle cases where n is odd.

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
;; ...devise a procedure that generates an iterative process for multiplying two
;; integers in terms of adding, doubling, and halving and uses a logarithmic 
;; number of steps.

;; lindseykuper:
;; The idea is that (a * b) is equal to ((a/2) * 2b), assuming /a/ is even.  So,
;; test if a = 1, and if so, we're done.  Otherwise, on each iteration, test for 
;; /a/'s evenness.  If /a/ is odd, subtract 1 from /a/ and add /b/ to the
;; accumulator (which again only exists to deal with odd cases).  If /a/ is even, 
;; halve /a/, double /b/, and call the procedure again with the resulting values. 
;; The final answer will be /b/ plus whatever's accumulated in the accumulator.

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
;;       a <--(becomes)-- a + b
;;       b <--(becomes)-- a
;; }
;;
;; T is a special case of T_pq where p = 0 and q = 1.
;; T_pq = {  
;;      a <--(becomes)-- bq + aq + ap
;;      b <--(becomes)-- bp + aq
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
;;      a <--(becomes)-- 2bpq + 2apq + 2(aq^2) + bq^2 + ap^2
;;      b <--(becomes)-- bp^2  + bq^2 + aq^2 + 2apq
;; }
;;
;; I think what we have to do now is figure out how what we have above fits into
;; the T_pq mold.  That is, we have to figure out what /p/ and /q/ have to be to
;; make the T_pq_TWICE transformation happen.

;; bq' + aq' + ap' = 2bpq + 2apq + 2(aq^2) + bq^2 + ap^2
;; bp' + aq'       = bp^2  + bq^2 + aq^2 + 2apq







(define (fib n)
  (fib-iter 1 0 0 1 n))
(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   <??>      ; compute p'
                   <??>      ; compute q'
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))


