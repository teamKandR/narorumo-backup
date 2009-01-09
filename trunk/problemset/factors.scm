;; Factors and Factorials

;; Problem statement taken from:
;; http://www.streamtech.nl/problemset/160.html

;; The factorial of a number N (written N!) is defined as the product
;; of all the integers from 1 to N. It is often defined recursively as
;; follows:

;; 1! = 1
;; N! = N * (N - 1)!

;; Factorials grow very rapidly -- 5! = 120, 10! = 3,628,800. One way
;; of specifying such large numbers is by specifying the number of
;; times each prime number occurs in it, thus 825 could be specified
;; as (0 1 2 0 1) meaning no twos, 1 three, 2 fives, no sevens and 1
;; eleven.

;; Write a program that will read in a number N (2 <= N <= 100) and
;; write out its factorial in terms of the numbers of the primes it
;; contains.

;; Input

;; Input will consist of a series of lines, each line containing a
;; single integer N. The file will be terminated by a line consisting
;; of a single 0.

;; Output

;; Output will consist of a series of blocks of lines, one block for
;; each line of the input. Each block will start with the number N,
;; right justified in a field of width 3, and the characters `!',
;; space, and `='. This will be followed by a list of the number of
;; times each prime number occurs in N!.

;; These should be right justified in fields of width 3 and each line
;; (except the last of a block, which may be shorter) should contain
;; fifteen numbers. Any lines after the first should be
;; indented. Follow the layout of the example shown below exactly.

;; Sample input

;; 5
;; 53
;; 0

;; Sample output

;;  5! =  3 1 1
;; 53! = 49 23 12 8 4 4 3 2 2 1 1 1 1 1 1
;;        1

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Solution:

;; One approach to solving this problem is to calculate the factorial,
;; find out that number's prime factors, then construct a list of
;; the multiplicity of each prime factor, and finally format the
;; output as specified in the problem statement.

;; Here's our factorial function, in continuation-passing style for
;; efficiency:

(define fact-cps
  (lambda (n k)
    (if (zero? n)
        (k 1)
        (fact-cps (- n 1) (lambda (v) (k (* n v)))))))

(define fact
  (lambda (n)
    (fact-cps n (lambda (v) v))))

;; Now, how to find the prime factors of a large number?  Well, if
;; it's prime, then it has only one prime factor -- itself.  If it's
;; composite, then we can use the following algorithm: find the
;; smallest prime divisor, divide the number by that, then look for
;; the prime factors of the quotient.

;; find-prime-factors: returns a list of the prime factors of n, in
;; sorted order from smallest to largest.
(define find-prime-factors
  (lambda (n)
    (if (prime? n)
        `(,n)
        (let ((spd (smallest-prime-divisor n)))
          (cons spd (find-prime-factors (/ n spd)))))))

;; smallest-prime-divisor: returns the smallest divisor of n.
(define smallest-prime-divisor
  (lambda (n)
    (find-divisor-with-incrementer n 2 next-prime)))

;; find-divisor-with-incrementer: similar to the SICP find-divisor
;; procedure, but with a twist.  It takes a third argument, inc, which
;; is a procedure of one argument that returns the next number to be
;; tested.
(define find-divisor-with-incrementer
  (lambda (n test-divisor inc)
    (cond
      [(> (square test-divisor) n) n]
      [(divides? test-divisor n) test-divisor]
      [else (find-divisor-with-incrementer n (inc test-divisor) next-prime)])))

;; next-prime: returns the next prime number strictly larger than n.
(define next-prime
  (lambda (n)
    (let [(next-int (+ 1 n))]
      (if (prime? next-int)
          next-int
          (next-prime next-int)))))

;; smallest-divisor, divides?, and square: helper procedures for the
;; prime? predicate and for find-divisor-with-incrementer.
(define smallest-divisor
  (lambda (n)
    (find-divisor-with-incrementer n 2 add1)))

(define divides?
  (lambda (a b)
    (= (remainder b a) 0)))

(define square
  (lambda (n)
    (* n n)))

(define prime?
  (lambda (n)
    (= n (smallest-divisor n))))

;; Now we can get a list of the prime factors of n! by calling
;; (find-prime-factors (fact n)).  Let's get that list into the proper
;; format.

;; multiplicity-list: takes a list of prime factors of the format
;; returned by find-prime-factors, and returns a list in the format
;; (<mult_1> <mult_2> <mult_3> ...) where mult_i is the multiplicity
;; of the ith prime.
(define multiplicity-list
  (lambda (ls)
    (cond
      ((null? ls) '())
      (else (let ((a (car ls)))
              (cons (count-occurrences ls a)
                    (multiplicity-list (remq a ls))))))))

;; count-occurrences: a helper for multiplicity-list.  Given a list ls
;; and and item x, returns the number of occurrences of x in ls.
(define count-occurrences
  (lambda (ls x)
    (cond
      ((null? ls) 0)
      ((= (car ls) x) (+ 1 (count-occurrences (cdr ls) x)))
      (else (count-occurrences (cdr ls) x)))))

;; print-multiplicity-list: takes a multiplicity list ls and a number
;; n and prints a line of formatted output.  (Chez Scheme supports
;; Common Lisp format directives for the printf function:
;; http://www.scheme.com/csug7/io.html#./io:s62).
(define print-multiplicity-list
  (lambda (ls n)
    (cond
      ((<= (length ls) 15)
       (printf "~3d! = ~{~2d~^ ~}~%" n ls))
      (else
       (print-multiplicity-list
        ;; list-head is available under Chez; some other
        ;; implementations use 'take'.
        (list-head ls 15) n)
       (print-subsequent-lines (list-tail ls 15))))))

;; print-subsequent-lines: a helper for print-multiplicity-list.  For
;; a multiplicity list of more than 15 numbers, prints the additional
;; numbers on new lines with slightly different formatting.
(define print-subsequent-lines
  (lambda (ls)
    (cond
      ((<= (length ls) 15)
       (printf "       ~{~2d~^ ~}~%" ls))
      (else
       (print-subsequent-lines (list-head ls 15))
       (print-subsequent-lines (list-tail ls 15))))))

;; process-number: given a number, prints a line (or lines) of
;; formatted output for that number.
(define process-number
  (lambda (n)
    (print-multiplicity-list
     (multiplicity-list (find-prime-factors (fact n)))n)))

;; Now we can have properly formatted output for any number n by
;; calling (process-number n).  All we have to do is handle file I/O,
;; and we're done.

;; read-line: not provided by Chez for some reason; borrowed from
;; http://www.cs.grin.edu/courses/Scheme/spring-1998/input-and-output.html
(define read-line
  (lambda ()
    (let loop ((next-char (read-char))
               (so-far '()))
      (if (char=? next-char #\newline)
          (list->string (reverse so-far))
          (loop (read-char) (cons next-char so-far))))))

;; number-list: read numbers from the file input.txt and store them in
;; a global list, number-list.
(define number-list
  (with-input-from-file "input.txt"
    (lambda ()
      (letrec ((kernel
                (lambda (ls)
                  (let ((next (peek-char)))
                    (if (or (eq? #\0 next)
                            (eof-object? next))
                        ls
                        (kernel (append ls (list (string->number
                                                  (read-line))))))))))
        (kernel '())))))

;; From the command line, use 'petite --script factors.scm' to run
;; under Petite Chez Scheme and exit.
(define main
  (lambda ()
    (for-each process-number number-list)))

(main)
