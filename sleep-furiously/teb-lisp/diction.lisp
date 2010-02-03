;; dealing with the words for this particular poem.

(defvar *dict* nil)

(defun words-to-dict (words)
  (let ((out (make-hash-table :test #'eq)))

    (loop for word in words do
	  (loop for pos in (poses word) do
		(setf (gethash pos out)
		      (union (list word)
			    (gethash pos out)))))
    out))


(defun restrict-beats (words max-beats nbeats)
  (remove-if 
   (if nbeats
       #'(lambda(x) (not (= (count-syllables x)
		       nbeats)))
     #'(lambda(x) (> (count-syllables x)
		     (or max-beats
			 most-positive-fixnum))))
   words))
	  

(defun pos-to-word (pos &key
			(max-beats most-positive-fixnum)
			rhymes-with
			butnot
			nbeats)
  (let ((words (restrict-beats (gethash pos *dict*)
			       max-beats nbeats)))

    ;; (format t "~a words left.~%" (length words))

    (cond
     ((null words)
      (progn ;; (format t "** no word of that pos!! **~%")
	nil))

     (rhymes-with
      (find-best-rhyme rhymes-with (set-difference words
						   butnot)))

     (t
      (pick-one words)))))

(defun pos-to-words (pos &optional (dict *dict*))
  (gethash pos dict))


(defun choose-words (poses &key rhymes-with butnot nbeats)
  (cond
   ((null poses) nil)
   
   ((= 1 (length poses))
    ;; (format t "one word left -- make it good.~%")
    (list (pos-to-word (car poses)
		       :nbeats nbeats
		       :butnot butnot
		       :rhymes-with rhymes-with)))

   (t (let ((nextword
	     (pos-to-word (car poses)
			  :max-beats (and nbeats
					  (- nbeats
					     (1- (length poses)))))))
				     
	(cond
	 ((null nextword) :backup)

	 (t (cons nextword
	       (choose-words (cdr poses)
			     :rhymes-with rhymes-with
			     :butnot butnot
			     :nbeats (and nbeats
					  (- nbeats
					     (count-syllables nextword)))))))
	))))

(defun use-dict (words)
  (setf *dict* (words-to-dict words)))

(defun use-dict-file (fn)
  (with-open-file
   (stream fn :direction :input)
   
   (use-dict (read stream))))

