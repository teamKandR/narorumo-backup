;; main.scm

;; Called from the "run" shell script with a hostname and a port.
;; Will fires up the rest of the controller.

(load "parse-from-mars.scm")
(load "controller.scm")

(define (main . args)
  (display args)
  (newline)
  (display "is this thing on?")
  (newline)
  
  (connect-with-args (car args) (read-from-string (cadr args)))
  (main-loop))