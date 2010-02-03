
(defun vowelp (c)
  (string-member c "aeiouy"))

(defun silentendp (letters)
  (or (and 
       (eq (car (last letters)) #\e)
       (eq (car (last (butlast letters))) #\l))
      (and
       (eq (car (last letters)) #\e)
       (eq (car (last (butlast letters))) #\s))))

(defun syllable-count (word &aux (cnt 0) prev)
  (let ((letters (listify (string-downcase word))))
    (cond
     ((zerop (length word)) 0)
     
     ((<= (length word) 3) 1)
     
     (t
      (loop for letter in letters do
	    (when (and (vowelp letter)
		       (not (vowelp prev)))
	      (incf cnt))
	    (setf prev letter))))
    
    (when (silentendp letters)
      (decf cnt))
    
    (when (zerop cnt) (incf cnt))
    
    cnt))