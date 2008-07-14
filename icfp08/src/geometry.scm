(define (deg->rad deg)
  (* (/ deg 180) pi))

(define (rad->deg rad)
  (* (/ rad pi) 180))  

(define (angle-dist angle1 angle2)
  (define (not-just-positives angle)
    (if (> angle 180)
        (- angle 360)
        angle))
  (let ((a1 (not-just-positives angle1))
        (a2 (not-just-positives angle2)))
    (min (abs (- a1 a2))
         (abs (- angle1 angle2)))))

(define (small-difference-cos? angle1 angle2 thresh)
  "Tells us whether or not the distance between ANGLE1 and ANGLE2 is within
   THRESH.  (For a given pair of angles, the lower THRESH is, the more likely
   we are to return #t.)"
  (> (cos (deg->rad (angle-dist angle1 angle2))) thresh))

(define (distance-between-points pair1 pair2)
  "Given two points, return the distance between them."
  (sqrt (+ (square (- (car pair1) (car pair2)))
           (square (- (cdr pair1) (cdr pair2))))))

(define (square n) (* n n))