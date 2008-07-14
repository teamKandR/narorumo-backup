(require scheme/tcp)
(require srfi/17) ;; generalized set!

(load "geometry.scm")
(load "parse-from-mars.scm")
(load "mars-map.scm")

(define SERVICE-PORT 17676)
(define SERVER-HOST  "127.0.0.1")

(define mars->earth null)
(define earth->mars null)
(define mm null)
(define current-strategy (cons 'go-home null))

(define (connect-with-args hostname port)
  (let-values (((in out)
                (tcp-connect hostname port)))
    (display in)
    (newline)
    (display out)
    
    (set! mars->earth in)
    (set! earth->mars out)
    (file-stream-buffer-mode mars->earth 'none)
    ))

(define (connect)
  (connect-with-args 
   SERVER-HOST
   SERVICE-PORT))

(define (setup-map init)
  (set! mm (build-map (/ (initialization-dx init) 2) 20)))

(define (main-loop)
  (let* ((input (read-until-semi mars->earth))
         (tokenlist (string->tokenlist input))
         (struct (tokenlist->struct tokenlist)))
    
    ;; maybe do setup.
    (when (initialization? struct)
      (setup-map struct))
    
    ;; perceive.
    ;; get the message, if it's a T, update the map.
    (if (telemetry? struct)
        (add-items mm (telemetry-objects struct))
        (begin
          (display struct)
          (newline)))
    
    ;; think and do.
    (when (telemetry? struct)
      (begin
        (when (anything-worrying? struct) 
          (set! current-strategy (cons 'evade current-strategy)))     
        (perform-action struct)))
    
    ;; again!
    (main-loop)))

(define (perform-action t) 
  (display (car current-strategy))
  (newline)
  
  (case (car current-strategy)
    ((evade) (evade t))
    ((go-home) (go-home t))))

(define (facing-home? t)
  (let ((currently-facing (telemetry-vehicle-dir t))
        (direction-home (direction-home t)))
    (small-difference-cos? currently-facing direction-home 0.95)))

(define (go-home t)
  (if (facing-home? t)
      (go-straight t)
      (turn-toward-home t)))

(define (go-straight t)
  "Move more or less in a straight line."
  (case (telemetry-vehicle-ctl t)
    ((aL bL -L)
     (begin (turn-right) (turn-right)))
    ((al bl -l)
     (turn-right))
    ((aR bR -R)
     (begin (turn-left) (turn-left)))
    ((ar br -r)
     (begin (turn-left))))
  (accelerate))

(define (evade t)
  "Evade objects in our way."
  ;; Pop the current-strategy stack.
  (set! current-strategy (cdr current-strategy))
  ;; Sophisticated evasion strategy.
  (accelerate-and-turn-left))

(define (send-message message)
  "Sends MESSAGE to Mars."
  (display message earth->mars)
  (display '\; earth->mars)
  (flush-output earth->mars))

(define (turn-left) (send-message 'l))
(define (turn-right) (send-message 'r))
(define (accelerate) (send-message 'a))
(define (brake) (send-message 'b))
(define (accelerate-and-turn-left) (send-message 'al))
(define (accelerate-and-turn-right) (send-message 'ar))
(define (brake-and-turn-left) (send-message 'bl))
(define (brake-and-turn-right) (send-message 'br))

(define (direction-to target-coords t)
  "Takes coordinates and a telemetry struct and returns the direction to those
   coordinates."
  (let* ((our-x (telemetry-vehicle-x t))
         (our-y (telemetry-vehicle-y t))
         (target-x (car target-coords))
         (target-y (cdr target-coords))
         (x-distance (- our-x target-x))
         (y-distance (- our-y target-y)))
    (+ 180 (rad->deg (atan y-distance x-distance)))))

(define (direction-home t)
  "Returns the direction toward home from our current position."
  (direction-to (cons 0 0) t))

(define (direction-to-object object t)
  "Returns the direction toward OBJECT from our current position."
  (let ((object-coords (cons (obj-x object) (obj-y object))))
    (direction-to object-coords t)))

(define (turn-toward-home t)
  "Takes a telemetry struct and points us toward home."
  (let ((currently-facing (telemetry-vehicle-dir t))
        (direction-home (direction-home t)))
    (send-message (direction-to-turn direction-home currently-facing))))

(define (direction-to-turn direction-we-want currently-facing)
  "Given DIRECTION-WE-WANT, the way we want to go, and CURRENTLY-FACING, the
   direction we're currently facing, tells us which way to turn to get there."
  (let ((diff 
         (- direction-we-want currently-facing)))
    (cond ((or (and (<= -360 diff) (<= diff -180))
               (and (<= 0 diff) (<= diff 180)))
           'l)
          ((or (and (<= -180 diff) (<= diff 0))
               (and (<= 180 diff) (<= diff 360)))
           'r)
          (else (error "Wrong parameters to direction-to-turn.")))))

(define (anything-worrying? t)
  "Tells us whether or not there are any objects in sight that we need to be
   worried about."
  (letrec ((objs (telemetry-objects t))
           (or-list (lambda (ls)
                      (if (null? ls)
                          #f
                          (or (car ls) (or-list (cdr ls)))))))
    (or-list (map (lambda (obj)
                    (need-to-be-worried? obj t))
                  objs))))

(define (need-to-be-worried? object t)
  "Tells us whether or not we need to worry about a particular object."
  (let* ((object-radius (get-radius object))
         (nearby (nearby? object (* 2.1 object-radius) t))
         (toward (moving-toward? object 0.6 t)))
    (and 
     (not (eq? 'h (obj-type object))) ;; We're not worried about home base!
     nearby 
     toward)))

(define (nearby? object tolerance t)
  "Tells us whether or not our distance from OBJECT is within TOLERANCE."
  (< (distance-from-object object t) tolerance))

(define (moving-toward? object tolerance t)
  "Tells us whether or not the direction we're currently heading is within
   TOLERANCE of the direction that OBJECT is in."
  (small-difference-cos? (direction-to-object object t) 
                         (telemetry-vehicle-dir t)
                         tolerance))

(define (get-radius object)
  "Returns the radius of OBJECT."
  (if (stationary? object)
      (stationary-radius object)
      1 ;; Pretend that Martians are 2m wide, because they can move.
      ))

(define (distance-from-object object t)
  "Returns our distance from the edge of OBJECT."
  (let ((distance-from-center (distance-from-center-of-object object t))
        (radius (get-radius object)))
    (- distance-from-center radius)))

(define (distance-from-center-of-object object t)
  "Returns our distance from the center of OBJECT."
  (let ((our-coords (cons (telemetry-vehicle-x t) (telemetry-vehicle-y t)))
        (their-coords (cons (obj-x object) (obj-y object))))
    (distance-between-points our-coords their-coords)))
