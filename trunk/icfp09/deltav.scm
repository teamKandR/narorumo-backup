;; gravitational constant
(define G 6.67428e-11)

;; mass of the earth
(define M 6.0e24)

(define pi 3.141592)

(define cube
  (lambda (n)
    (* n n n)))

;; How far does a (circularly) orbiting body travel in a given time
;; step?
(define rps ;; radians per second
  (lambda (r)
    (- (/ 1 (sqrt (/ (cube r) (* G M)))))))

;; Given current coordinates, where will a (circularly) orbiting body
;; be at time t from now (in radians)?
(define pos-rad
  (lambda (x y t)
    (let* ([r (orbit-radius x y)]
           [current-radians (atan y x)]
           [rps (rps r)])
      (+ current-radians (* rps t)))))

;; Wikipedia is amazing.
(define hohmann-transfer-time
  (lambda (r1 r2)
    (* pi (sqrt (/ (cube (+ r1 r2)) (* 8 G M))))))

(define (firstthrust r1 r2)
  (let ((leftpart (sqrt (/ (* G M) r1)))
        (rightpart (- (sqrt (/ (* 2 r2) (+ r1 r2))) 1)))
    (* leftpart rightpart)))

(define (secondthrust r1 r2)
  (let ((leftpart (sqrt (/ (* G M) r2)))
        (rightpart (- 1 (sqrt (/ (* 2 r1) (+ r1 r2))))))
    (* leftpart rightpart)))

(define (pythag x y)
  (sqrt (+ (* x x) (* y y))))

(define (deltav-x sx0 sy0 sx1 sy1 r1 r2)
  (let* ((vx (- sx1 sx0))
         (vy (- sy1 sy0))
         (speed (pythag vx vy))
         (deltav (firstthrust r1 r2)))
    (- (* (/ vx speed) deltav))))

(define (deltav-y sx0 sy0 sx1 sy1 r1 r2)
  (let* ((vx (- sx1 sx0))
         (vy (- sy1 sy0))
         (speed (pythag vx vy))
         (deltav (firstthrust r1 r2)))
    (- (* (/ vy speed) deltav))))

(define (deltavp-x sx0 sy0 sx1 sy1 r1 r2)
  "Get the x component of the second thrust for your Hohmann transfer."
  (let* ((vx (- sx1 sx0))
         (vy (- sy1 sy0))
         (speed (pythag vx vy))
         (dvp (secondthrust r1 r2)))
    (* (/ vx speed) dvp)))

(define (deltavp-y sx0 sy0 sx1 sy1 r1 r2)
  "Get the y component of the second thrust for your Hohmann transfer."
  (let* ((vx (- sx1 sx0))
         (vy (- sy1 sy0))
         (speed (pythag vx vy))
         (deltav (secondthrust r1 r2)))
    (* (/ vy speed) deltav)))
