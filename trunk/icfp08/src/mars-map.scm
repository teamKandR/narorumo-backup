(load "structs.scm")

(require srfi/1) ;; lists
(require srfi/43) ;; vectors

(define-struct mars-map
  (boxes box-size size n-boxes-across))

;; Map will be of dimensions 2*size x 2*size, covered in box-size square boxes.
;; so "size" is really more like "max in a direction."
(define (build-map size box-size)
  (let* ((n-boxes-across (inexact->exact (floor (/ (* 2 size) box-size))))
         (box-vector (make-vector (* n-boxes-across n-boxes-across) null)))
    (make-mars-map box-vector box-size size n-boxes-across)))

;; Add the specified item to the mars map, in the right place.
;; item must be of type stationary.
(define (add-item mm item)
  (when (stationary? item)
    (let* ((box-x (coord->box-coord mm (obj-x item)))
           (box-y (coord->box-coord mm (obj-y item))))
      (put-item-in-box mm box-x box-y item))))

(define (add-items mm items)
  (map (lambda (item) (add-item mm item))
       items))

;; Works for both x and y. Pass a map and a value, returns box coordinate
(define (coord->box-coord mm x)
  (let* ((pos-x (+ (mars-map-size mm) x))
         (box-x (inexact->exact (floor (/ pos-x (mars-map-box-size mm))))))
    box-x))

(define (box-index mm box-x box-y)
  (when (or (not (exact? box-x))
            (not (exact? box-y)))
    (error "inexact numbers!!"))
  (+ (* box-y (mars-map-n-boxes-across mm)) box-x))

;; in box coordinates
(define (box-at-boxpos mm box-x box-y)
  (vector-ref (mars-map-boxes mm)
              (box-index mm box-x box-y)))

;; in screen coordinates.
(define (box-at-pos mm x y)
  (box-at-boxpos (coord->box-coord mm x)
                 (coord->box-coord mm y)))

(define (put-item-in-box mm box-x box-y item)
  (let* ((index (box-index mm box-x box-y))
         (box (vector-ref (mars-map-boxes mm) index)))

    (unless (member item box)
      (vector-set! (mars-map-boxes mm) index (cons item box)))
    (vector-ref (mars-map-boxes mm) index)))

;; Given some coordinates and a radius, what are all the items that are in the
;; bounding box of that radius?
(define (whats-nearby mm x y radius)
  (let ((lower-box-x (max 0 (coord->box-coord mm (- x radius))))        
        (lower-box-y (max 0 (coord->box-coord mm (- y radius))))
        (upper-box-x (min (mars-map-n-boxes-across mm)
                          (coord->box-coord mm (+ x radius))))
        (upper-box-y (min (mars-map-n-boxes-across mm)
                          (coord->box-coord mm (+ y radius)))))
    
    (apply append
           (map (lambda (lst-x-y)
                  (box-at-boxpos mm (car lst-x-y) (cadr lst-x-y)))
                (cross
                 (range lower-box-x upper-box-x)
                 (range lower-box-y upper-box-y))))))

(define (cross xs ys)
  (apply append
         (map (lambda(x)
                (map (lambda(y) (list x y))
                     ys))
              xs)))

(define (range low high)
  (cond
    ((= low high) '())
    (else (cons low (range (+ 1 low) high)))))

;(define mm (build-map 100 20))
;(define b (make-stationary 'b -5 10 10))
;(define b2 (make-stationary 'c 10 19 10))
;(add-item mm b2)
;(add-item mm b)
;
