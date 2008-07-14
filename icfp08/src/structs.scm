;; structs.scm
;; Exciting and useful datastructures for the life of a robot on Mars.

;;; Events.
(define-struct initialization
  (dx dy time-limit min-sensor max-sensor max-speed max-turn
      max-hard-turn))

(define-struct telemetry
  (time-stamp vehicle-ctl vehicle-x vehicle-y
              vehicle-dir vehicle-speed objects))

(define (telemetry time-stamp vehicle-ctl vehicle-x vehicle-y
              vehicle-dir vehicle-speed objects)
  (let ((pos-dir
         (if (< vehicle-dir 0) (+ vehicle-dir 360) vehicle-dir)))
    (make-telemetry time-stamp vehicle-ctl vehicle-x vehicle-y
              pos-dir vehicle-speed objects)))

(define-struct unhappy
  (type timestamp))

(define-struct success
  (timestamp))

(define-struct end
  (timestamp score))

;;; Objects
(define-struct obj
  (type x y))

(define-struct (enemy obj)
  (dir speed))

;; But I don't WANT to make enemies!
(define (enemy type x y dir speed)
  (let ((pos-dir
         (if (< dir 0) (+ dir 360) dir)))
    (make-enemy type x y pos-dir speed)))              

(define-struct (stationary obj)
  (radius))
