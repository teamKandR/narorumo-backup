;; Stuff that all our controllers will need.

(load "vm.scm")

;; flip: changes an alist from, e.g., ((a . 1) (b. 2) (c . 3)) to
;; ((1 . a) (2 . b) (3 . c))
(define flip
  (lambda (alist)
    (map (lambda (pr)
           (let ([a (car pr)]
                 [d (cdr pr)])
             `(,d . ,a)))
         alist)))

(define read-output-port
  (lambda (port-name)
    (let ([port-number (cdr (assq port-name (flip output-port-mapping)))])
      (cond
        [(assq port-number output-ports) => cdr]
        [else 0]))))

(define read-input-port
  (lambda (port-name)
    (let ([port-number (cdr (assq port-name (flip input-port-mapping)))])
      (cdr (assq port-number input-ports)))))

(define write-input-port!
  (lambda (port-name value)
    (let ([port-number (cdr (assq port-name (flip input-port-mapping)))])
      (set-cdr! (assq port-number input-ports) value))))

(define log-input
  (lambda (n p)
    (fprintf p "\n-------------------------------------------\n")
    (fprintf p
             "Time step ~d of scenario ~d\n\n"
             n
             (cdr (assq 16000 input-ports)))
    (fprintf p "Inputs to VM:\n")
    (map (lambda (pr)
           (let* ([a (car pr)]
                  [d (cdr pr)]
                  [port-meaning (cdr (assq a input-port-mapping))])
             (fprintf p "~19s: ~@f\n" port-meaning d)))
         input-ports)
    (fprintf p "\n")))

;; machine-readable format for traces.
(define (log-input-trace step out)
  (fprintf out "~d ~d\n" step (length input-ports))
  (map (lambda (port-pair)
         (let* ([portnum (car port-pair)]
                [portval (cdr port-pair)])
           (fprintf out "~d ~f\n" portnum portval)))
       input-ports))

(define earth-radius 6.357e6)

;; Also known as the Pythagorean theorem.
(define orbit-radius
  (lambda (s_x s_y)
    (sqrt (+ (* s_x s_x) (* s_y s_y)))))

(define (altitude sx sy)
  "Distance from the center of the earth minus its radius."
  (- (orbit-radius sx sy) earth-radius))

(define (log-location step sx sy out)
  (fprintf out "~0d, ~0d, ~0d, ~0d\n" step
           (floor sx) (floor sy) (floor (altitude sx sy))))

(define log-output
  (lambda (n p ls)
    ;; Log problem-specific stuff
    (pmatch ls
      [(hohmann ,cor ,tor)
       (fprintf p "Current orbit radius: ~f\n"  cor)
       (fprintf p "Orbit radius difference: ~f\n\n" (- cor tor))]
      [(meetandgreet ,cor ,tor ,s_x ,s_y)
       (fprintf p "Current orbit radius: ~f\n"  cor)
       (fprintf p "Target orbit radius: ~f\n"  tor)
       (fprintf p "x-coordinate of target: ~f\n"  s_x)
       (fprintf p "y-coordinate of target: ~f\n"  s_y)
       (fprintf p "Orbit radius difference: ~f\n\n" (- cor tor))]
      [(emeetandgreet) (void)]
      [(clearskies) (void)])

    ;; Log all output ports
    (fprintf p "Outputs from VM:\n")
    (map (lambda (pr)
           (let* ([a (car pr)]
                  [d (cdr pr)]
                  [port-meaning (cdr (assq a output-port-mapping))])
             (fprintf p "~19s: ~@f\n" port-meaning d)))
         output-ports)))

(define show-visualization
  (lambda (problem-name)
    (let ([command (string-append
                    "R CMD BATCH "
                    (symbol->string problem-name)
                    "."
                    (number->string (read-input-port 'configuration))
                    ".R")])
      (system command)
      (system "open Rplots.pdf"))))
