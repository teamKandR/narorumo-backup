(load "lib/pmatch.scm")

(define status-reg #f)

(define the-instructions 'foo)
(define the-heap 'foo)

(define input-ports
  '())

(define output-ports
  '())

(define imm-shift
  (lambda (imm)
    (pmatch (ash imm -7)
      ;; the five possible imm opcodes
      [0 <]
      [1 <=]
      [2 =]
      [3 >=]
      [4 >])))

(define populate-memory!
  (lambda (fn)
    (let* ([p (open-input-file fn)]
           [ls (read p)])
      (set! the-instructions (list->vector (cdar ls)))
      (set! the-heap (list->vector (cdadr ls))))))

(define populate-input-ports!
  (lambda (ls)
    (set! input-ports ls)))

(define execute-instruction!
  (lambda (inst pc)
    (pmatch inst
      ;; s-type instructions
      [(noop ,r1) (void)]
      [(cmpz ,imm ,r1)
       (let ([r1 (vector-ref the-heap r1)])
         (set! status-reg ((imm-shift imm) r1 0.0)))]
      [(sqrt ,r1)
       (vector-set! the-heap pc
                    (sqrt (vector-ref the-heap r1)))]
      [(copy ,r1)
       (vector-set! the-heap pc
                    (vector-ref the-heap r1))]
      [(input ,r1)
       (vector-set! the-heap pc (cdr (assq r1 input-ports)))]
      ;; d-type instructions
      [(add ,r1 ,r2)
       (vector-set! the-heap pc
                    (+ (vector-ref the-heap r1)
                       (vector-ref the-heap r2)))]
      [(sub ,r1 ,r2)
       (vector-set! the-heap pc
                    (- (vector-ref the-heap r1)
                       (vector-ref the-heap r2)))]
      [(mult ,r1 ,r2)
       (vector-set! the-heap pc
                    (* (vector-ref the-heap r1)
                       (vector-ref the-heap r2)))]
      [(div ,r1 ,r2)
       (vector-set! the-heap pc
                    (if (= (vector-ref the-heap r2) 0.0)
                        0.0
                        (/ (vector-ref the-heap r1)
                           (vector-ref the-heap r2))))]
      [(output ,r1 ,r2)
       (let ([p (assq r1 output-ports)]
             [r2 (vector-ref the-heap r2)])
         (if p
             (set-cdr! p r2)
             (set! output-ports (cons `(,r1 . ,r2) output-ports))))]
      [(phi ,r1 ,r2)
       (vector-set! the-heap pc
                    (if status-reg
                        (vector-ref the-heap r1)
                        (vector-ref the-heap r2)))])))

(define vm
  (lambda ()
    (begin
      (let ([n (vector-length the-instructions)])
        (letrec ([loop (lambda (pc)
                         (if (not (= pc n))
                             (begin
                               (execute-instruction!
                                (vector-ref the-instructions pc)
                                pc)
                               (loop (add1 pc)))))])
          (loop 0))))))
