(defun string-member (c str)
  (some #'(lambda(x) (eq c x)) str))

(defun listify (seq)
  (loop for i from 0 below (length seq)
        collect (elt seq i)))

(defun arrayify (seq &optional (type t))
  (make-array (length seq)
	      :initial-contents seq
	      :element-type type))

(defun stringify (seq)
  (arrayify seq 'character))

(defun slice (seq &optional (start 0) (offset (length seq)))
  (loop for i from start
	below (min (+ start offset)
		   (length seq))
	collect (elt seq i)))

(defun string-slice (str &optional (start 0) (offset (length str)))
  (stringify (slice (listify str) start offset)))

(defun string-startswith (str start)
  (equalp (string-slice str 0 (length start))
	  start))

(defun string-endswith (str end)
  (string-startswith (reverse str) (reverse end)))

;; levenshtein algorithm from WikiSource
;; http://en.wikisource.org/wiki/Levenshtein_distance
(defun levenshtein-distance (a b)
  "Computes the Levenshtein distance between strings a and b."
  (let ((al (length a))
        (bl (length b)))
    ;; Swap a and b if a is longer.
    (when (> al bl)
      (multiple-value-setq (a b al bl) (values b a bl al)))
    (loop
       for i below bl
       for prev = (loop for i from 0 to al collect i)
                then (copy-list current)
       for current = (cons (1+ i) (make-list al :initial-element 0))
       do (loop
             for j below al
             for (c0 . ccons) on current
             for (p0 p1) on prev
             for add = (1+ p1)
             for delete = (1+ c0)
             for change = (+ p0 (if (eql (elt a j) (elt b i))
                                    0 1))
             do (rplaca ccons (min add delete change)))
       finally (return (car (last current))))))

(defun pick-one (things)
  (elt things (random (length things))))
