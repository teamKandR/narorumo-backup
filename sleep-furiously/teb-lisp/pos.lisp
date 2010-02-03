(defvar *pos-dict*
  (make-hash-table :test #'eq))

(defparameter *pos-dict-file* "moby-pos.sexprs")

(defvar *line* 0)

(defun load-pos-dictionary ()
  (clrhash *pos-dict*)

  (with-open-file
   (stream *pos-dict-file* :direction :input)
   (do ((sexpr (read stream nil 'eof)
	       (read stream nil 'eof)))
       ((eql sexpr 'eof))

     (setf (gethash (car sexpr) *pos-dict*)
	   (append (cdr sexpr)
		   (gethash (car sexpr) *pos-dict*)))))

  "okay!")

(defun poses (word)
  (gethash word *pos-dict*))