-- Structure and Interpretation of Computer Programs Exercises
-- Chapter 1: Haskell Edition
-- Alex Rudnick and Lindsey Kuper

---- 1.2
-- Translate the following expression into prefix form.
--
-- The Scheme looks like this:
-- (define (one-point-two)
--   (/ (+ 5 4 (- 2 (- 3 (+ 6 4/5))))
--      (* 3 (- 6 2) (- 2 7))))

onepointtwo_prefix = (/) ((+) ((+) 5 4) ((-) 2 ((-) 3 ((+) 6 ((/) 4 5)))))
                          ((*) ((*) 3 ((-) 6 2)) ((-) 2 7))

-- Or, more naturally in Haskell:
top = (5 + 4 + (2 - (3 - (6 + 4/5))))
bottom = (3 * (6 - 2) * (2 - 7))
onepointtwo = top / bottom

---- 1.3
-- Define a procedure that takes three numbers as arguments and returns
-- the sum of squares of the two larger numbers.

square x = x * x
sum_of_squares a b = square a + square b

ss_of_largest_two a b c
  | (a < b) && (a < c) = sum_of_squares b c
  | (b < a) && (b < c) = sum_of_squares a c

-- else --
ss_of_largest_two a b c = sum_of_squares a b

---- 1.4
-- Going ahead and writing a-plus-abs-b in Haskell.
a_plus_abs_b a b = (if (b > 0) then (+) else (-)) a b

-- 1.5
---- Ben Bitdiddle defines the following procedures...

p = p
test x y = if (x == 0) then 0 else y

-- He then evaluates the expression (test 0 (p)).

-- alexr: In this case, Haskell's lazy evaluation just returns 0. Since x is 0,
-- we don't need to bother finding out what the value of running p is. (it does
-- turn out to be an infinite loop if you run it on its own)
