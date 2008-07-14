(require srfi/69) ;; hash tables
(require srfi/13) ;; strings
(require srfi/14) ;; char sets
(require srfi/1)  ;; lists

(require mzlib/string)

(load "structs.scm")

;; In this file, we will build data structures that describe
;; messsages sent from the server. They will be:

;; initialization: a message starting with "I".
;; telemetry: a message starting with "T"
;; adverse: B, C, or K.
;; success: S
;; end: E

(define stationary-x obj-x)
(define stationary-y obj-y) 

(define (tokenlist->struct tokenlist)
  "Take a list of tokens and produce the structure for the
   appropriate kind of message"
  (case (car tokenlist)
    ((I) (apply make-initialization (cdr tokenlist)))
    ((B C K) (apply make-unhappy tokenlist))
    ((S) (apply make-success (cdr tokenlist)))
    ((E) (apply make-end (cdr tokenlist)))
    ((T) (apply telemetry
                (append (take (cdr tokenlist) 6)
                        (list (list-objects (drop (cdr tokenlist) 6))))))
    (else 'other)))

(define (msgstring str)
  "Assume there's only one tokenizable string in here."
  (let ((semi-loc (string-contains str ";")))
    (if semi-loc
        (string-take str semi-loc)
        null)))

(define (msgstrings str)
  "Return a list of strings that we can tokenize; semis at
   the end are stripped out."
  (let ((semi-loc (string-contains str ";")))
    (if semi-loc
        (cons (string-take str semi-loc)
              (msgstrings (string-drop
                           str
                           (+ 1 semi-loc ))))
        null)))

(define I "I 1 2 3 4 5 6 7 8 ;" )
(define T "T 3450 aL -234.040 811.100 47.5 8.450 b -220.000 750.000 12.000 m -240.000 812.000 90.0 9.100 ;")

(define objects '(b -220.0 750.0 12.0 m -240.0 812.0 90.0 9.1 m -240.0 812.0 90.0 9.1 b -220.0 750.0 12.0 c -220.0 750.0 12.0))

(define (string->tokenlist str)
  "There's only one message in this string; extract it"
  (map read-from-string (string-tokenize (msgstring str))))

(define (string->tokenlists str)
  (map (lambda (strings)
         (map read-from-string strings))
       (map string-tokenize (msgstrings str))))

(define (list-objects obj-tokens)
  (if (empty? obj-tokens) null
      (case (car obj-tokens)
        ((b c h) (cons (apply make-stationary (take obj-tokens 4))
                       (list-objects (drop obj-tokens 4))))
        ((m) (cons (apply enemy (take obj-tokens 5))
                   (list-objects (drop obj-tokens 5)))))))

(define (find-semi-in-stream in)
  ;; "Keep peeking until we've found a semi, then return the position."
  (define (keep-peeking skip seen)
    (let* ((peeked (string-append seen
                                 (peek-string 100 skip in)))
           (semi-pos (string-contains peeked ";")))
      (if semi-pos
          (+ 1 semi-pos) ;; We want to read in the semi too.
          (keep-peeking (+ skip 100) peeked))))             
  (keep-peeking 0 ""))

(define (read-until-semi in)
  "Grab one message from Mars, presumably quickly!"
  (let ((semi-pos (find-semi-in-stream in)))
    (read-string semi-pos in)))

(define (read-until-semi-principled in)
  "Grab one message from Mars."
  (let kernel ((chars '())
               (next (peek-char in)))
    (cond ((eof-object? next)
           (list->string (reverse chars)))
          ((char=? next #\;)
           (read-char in)
           (list->string (reverse chars)))
          (else
           (kernel (cons (read-char in) chars) (peek-char in))))))
