;; Structure and Interpretation of Computer Programs
;; Exercise 1.19
;; Lindsey Kuper
;; May 12, 2008

(define (square x) (* x x)) ; so this will run under a bare-bones R5RS

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Test cases:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(display (fib 13))        ; 233

;(display (fib 20))        ; 6765

;(display (fib 100))       ; 354224848179261915075

;(display (fib 1000000))   ; The millionth Fibonacci number.
                           ; http://www.upl.cs.wisc.edu/~bethenco/fibo/

;(display (fib 10000000))  ; The ten millionth Fibonacci number.
                           ; http://www.bigzaphod.org/fibonacci/

;(display (fib 100000000)) ; The hundred millionth Fibonacci number.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Explanation:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Here's the transformation /T/ that the exercise refers to:

;; T = {
;;       a -> a + b
;;       b -> a
;; }

;; As the exercise points out, though, /T/ is really just a special case of the
;; transformation /T_pq/ 

;; T_pq = {  
;;      a -> bq + aq + ap
;;      b -> bp + aq
;; }

;; in which /p/ = 0 and /q/ = 1.

;; In general, what happens when we apply /T_pq/ twice?  Well, in the first
;; application of /T_pq/, we get

;; a = bq + aq + ap 
;; b = bp + aq

;; In the second application, if we plug these new values of /a/ and /b/ back
;; into /T_pq/, we get

;; new a = (bp + aq)q + (bq + aq + ap)q + (bq + aq + ap)p
;; new b = (bp + aq)p + (bq + aq + ap)q

;; Multiplied out, that turns out to be:

;; new a = 2bpq + 2apq + 2(aq^2) + bq^2 + ap^2
;; new b = bp^2  + bq^2 + aq^2 + 2apq
 
;; So, we've got a new transformation which I'll call /T_pq_TWICE/.

;; T_pq_TWICE = {
;;      a -> 2bpq + 2apq + 2(aq^2) + bq^2 + ap^2
;;      b -> bp^2  + bq^2 + aq^2 + 2apq
;; }

;; Now, the exercise says that this new transformation /T_pq_TWICE/ is supposed to
;; be "of the same form" as _T_pq_.  What does that mean?  To me, it means that
;; the expression "2bpq + 2apq + 2(aq^2) + bq^2 + ap^2" somehow "fits the mold" of
;; "bq + aq + ap", and the expression "bp^2  + bq^2 + aq^2 + 2apq" fits the mold
;; of "bp + aq".  

;; When I say "fits the mold", I mean there there exist some values for /p/ and
;; /q/ that we can plug into the expressions "bq + aq + ap" and "bp + aq" and
;; make them come out to equal "2bpq + 2apq + 2(aq^2) + bq^2 + ap^2" and 
;; "bp^2  + bq^2 + aq^2 + 2apq", respectively.

;; If we use some notation to tell our "old" and "new" /p/s and /q/s apart, we can
;; express this in equation form.  So, if we use /p'/ instead of /p/ and /q'/
;; instead of /q/ in the original two expressions, we get:

;; bq' + aq' + ap' = 2bpq + 2apq + 2(aq^2) + bq^2 + ap^2      (1)
;; bp' + aq'       = bp^2  + bq^2 + aq^2 + 2apq               (2)

;; And suddenly, we have a system of equations which we can use to solve for /p'/
;; and /q'/ in terms of /p/ and /q/!  I did it by subtracting equation (2) from
;; equation (1) and solving for /q'/:

;; ap' + bq' - bp'    = ap^2 + 2bpq  + aq^2 - bp^2
;; a(p') + b(q' - p') = a(p^2 + q^2) + b(2pq - p^2)
;;       p' + q' - p' = p^2 + q^2 + 2pq - p^2
;;                 q' = q^2 + 2pq

;; Then I substituted this value of /q'/ back into equation (1) to solve for /p'/:

;; bp' + a(q^2 + 2pq) = bp^2  + bq^2 + aq^2 + 2apq  
;; bp' + aq^2 + 2apq  = bp^2  + bq^2 + aq^2 + 2apq  
;;                bp' = bp^2  + bq^2
;;                 p' = p^2 + q^2

;; To make sure my algebra was right, I substituted those values for /p'/ and
;; /q'/ back into the /T_pq/ transformation to see if the resulting values for
;; /a/ and /b/ would be the same as applying /T_pq_TWICE/ on regular /p/ and /q/:

;; a = bq + aq + ap
;; a = b(q^2 + 2pq) + a(q^2 + 2pq) + a(p^2 + q^2)
;; a = bq^2 + 2bpq + aq^2 + 2apq + ap^2 + aq^2 
;; (Yes, this equals what we have in T_pq_TWICE.)

;; b = bp + aq
;; b = b(p^2 + q^2) + a(q^2 + 2pq)
;; b = bp^2 + bq^2 + aq^2 + 2apq
;; (Yes, this equals what we have in T_pq_TWICE.)

;; Great!  So I had formulas for /p'/ and /q'/:

;;                 p' = p^2 + q^2
;;                 q' = q^2 + 2pq

;; In Scheme, these expressions look like:

;;                 (+ (square p) (square q))
;;                 (+ (square q) (* 2 p q))

;; I plugged in those expressions on the "complete /p'/" and "complete /q'/"
;; lines of the procedure, and I was done.