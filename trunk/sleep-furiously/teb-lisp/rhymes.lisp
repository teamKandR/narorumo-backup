(defparameter *sound-dict-file* "cmudict.0.6")

(defvar *sound-dict*
  (make-hash-table :test #'eq))

(defun add-line-to-dict (line)
  (let ((listform
	 (read-from-string (format nil "(~a)" line)
			   nil 'eof)))

    (setf (gethash (car listform) *sound-dict*)
	  (cons (remove-if #'listp (cdr listform))
		(gethash (car listform) *sound-dict*)))))

(defun load-sound-dictionary ()
  (with-open-file
   (stream *sound-dict-file* :direction :input)
   (do ((line (read-line stream nil 'eof)
	      (read-line stream nil 'eof)))
       ((eql line 'eof))
     (unless (string-startswith line "##")
       (add-line-to-dict line))))

  "okay!")
       
(defun vowelp (sym)
  (let ((str (symbol-name sym)))
    (digit-char-p (aref str (1- (length str))))))

(defun remove-stress (vowel-sym)
  (multiple-value-bind (out ignore)
      (intern (string-right-trim "012" (symbol-name vowel-sym)))
    ignore
    out))

(defun count-syllables (sounds)
  (if (listp sounds)
      (count-if #'vowelp sounds)
    (count-syllables (car (gethash sounds *sound-dict*)))))

;; ** syllable splitting!
;; for each vowel phoneme in the word:
;;         label the phoneme with a unique integer, starting a zero
;; for each consonant phoneme in the word:
;;         find the closest vowel phonemes to this consonant
;;         if there is a single closest phoneme, copy its label to this phoneme
;;         if there are two closest phonemes:
;;                 copy the label of the phoneme with a higher stress
;;         if there are two closest phonemes and with the same stress:
;;                 copy the label of the phoneme on the right (this is
;;                 arbitrary but a  tiebreaker is needed, and this seems to
;;                 work better than copying the label on the left).

(defun closest-vowel-posns (sound-array pos)
  (let* ((posns
	  (loop for i from 0 below (length sound-array)
		when (vowelp (aref sound-array i))
		collect i))
	 (sorted (sort posns
	       #'(lambda(x y)
		   (< (abs (- x pos))
		      (abs (- y pos)))))))
    (cond
     ((= 1 (length sorted)) sorted)
     ((= (abs (- (first sorted) pos))
	 (abs (- (second sorted) pos))) (subseq sorted 0 2))
     (t (subseq sorted 0 1)))))
	 

;; given a vowel symbol, return its stress.
(defun vowel-stress (sym)
  (let ((str (symbol-name sym)))
    (aref str (1- (length str)))))

(defun syllable-vowel-sound (syllable)
  (remove-stress (find-if #'vowelp syllable)))

;; given a syllable (as a list), return its stress
(defun syllable-stress (syllable)
  (vowel-stress (find-if #'vowelp syllable)))

(defun syllable-prefix (syllable)
  (loop for sym in syllable until (vowelp sym)
	collect sym))

(defun syllable-suffix (syllable)
  (reverse
   (loop for sym in (reverse syllable) until (vowelp sym)
	collect sym)))

;; higher stress given a pair of closest indices...
(defun higher-stress (closests sound-array)
  (let* ((firststress (vowel-stress (aref sound-array
					 (first closests))))
	 (secondstress (vowel-stress (aref sound-array
					   (second closests)))))

    (cond
     ((char-greaterp firststress secondstress) (first closests))
     (t (second closests))))) ;; give preference to second vowel, as
                              ;; it "seems to work better"

(defun label-consonant (sound-array label-array pos)
  (let ((closests (closest-vowel-posns sound-array pos)))

    (cond
     ((= 1 (length closests))
      (setf (aref label-array pos)
	    (aref label-array (car closests))))

     ((= 2 (length closests))
	 
      (setf (aref label-array pos)
	    (aref label-array (higher-stress closests sound-array)))))))
   

(defun sounds-to-syllables (sounds)
  (let ((sound-array (arrayify sounds))
	(label-array (make-array (length sounds)))
	(nvowels -1))

    ;; label the vowels
    (loop for i from 0 below (length sounds) do
	  (when (vowelp (aref sound-array i))
	    (setf (aref label-array i) (incf nvowels))))

    ;; label the consonants
    (loop for i from 0 below (length sounds) do
	  (unless (vowelp (aref sound-array i))
	    (label-consonant sound-array label-array i)))

    (loop for syllable-label from 0 upto nvowels
	  collect (let* ((start (position syllable-label
					 label-array))
			 (end (+ start
				 (count syllable-label label-array))))
		    (subseq (listify sound-array) start end)))))
			  
(defun word-to-syllables (word)
  (mapcar #'sounds-to-syllables (gethash word *sound-dict*)))

(defparameter *search-breadth* 0.3
  "controls how many rhyme pairs we explore") ;; probably don't need.

(defparameter *search-depth* 0.5
  "controls how long found rhymes will be") ;; also probably don't need

(defparameter *search-quality* 0.75 "threshhold for rhyming")

(defparameter *points-vowel-sound* 8)
(defparameter *points-vowel-stress* 1)
(defparameter *points-suffix* 6)
(defparameter *points-prefix* 3)

;; rhyme detection!!

;; initialize intermediate and possible to 0.
;; add PointsVowelStress points to possible
;; if syllable 'a' and syllable 'b' have the same stress:
;; 	add PointsVowelStress points to intermediate
;; 	add PointsVowelSound points to possible

(defun rhyme-score (syl-a syl-b)
  (let ((scored 0)
	(possible 0)
	(pref-a (syllable-prefix syl-a))
	(pref-b (syllable-prefix syl-b))
	(suff-a (syllable-suffix syl-a))
	(suff-b (syllable-suffix syl-b)))

    (incf possible *points-vowel-stress*)
    (incf possible *points-vowel-sound*)

    (when (eq (syllable-stress syl-a)
	      (syllable-stress syl-b))
      (incf scored *points-vowel-stress*))

    (when (eq (syllable-vowel-sound syl-a)
	      (syllable-vowel-sound syl-b))
      (incf scored *points-vowel-sound*))

    (incf possible *points-prefix*)
    (unless (and (zerop (length pref-a))
		 (zerop (length pref-b)))
      (incf scored (* *points-prefix*
		      (- 1 (/ (levenshtein-distance pref-a pref-b)
			      (max (length pref-a) (length pref-b)))))))

    (incf possible *points-suffix*)
    (unless (and (zerop (length suff-a))
		 (zerop (length suff-b)))
      (incf scored (* *points-suffix*
		      (- 1 (/ (levenshtein-distance suff-a suff-b)
			   (max (length suff-a) (length suff-b)))))))


    (/ scored possible)))

;; let prefix_distance equal the "edit distance" between the prefix of a and
;;     the prefix of b
;; let prefix_max equal the total number of phonemes in the prefix of a +
;;     the total number of phonemes in the prefix of b
;; let prefix_similarity equal prefix_distance / prefix_max

;; add prefix_similarity * PointsPrefix to intermediate
;; add PointsPrefix to Possible

;; compute suffix_distance, suffix_max, suffix_similarity in the same way
;; add suffix similarity * PointsSuffix to intermediate
;; add PointsSuffix to Possible

;; calculate the final score by dividing intermediate by Possible 


(defun best-rhyme-score (word-sounds otherword)
  (let ((other-prons (word-to-syllables otherword)))
    (loop for pron in word-sounds
	  maximize
	  (loop for other-pron in other-prons
		maximize (rhyme-score (car (last other-pron))
				      (car (last pron)))))))
  
(defun find-best-rhyme (word possibilities
			      &optional word-sounds sofar sofar-score)
  
  (cond
   ((null possibilities) sofar)

   ((null sofar)
    (find-best-rhyme word
		     (cdr possibilities)
		     (word-to-syllables word)
		     (car possibilities)
		     (best-rhyme-score (word-to-syllables word)
				       (car possibilities))))

   (t
    (let ((newscore (best-rhyme-score word-sounds (car possibilities))))
      (if (> newscore sofar-score)
	  (find-best-rhyme word
				(cdr possibilities)
				word-sounds (car possibilities) newscore)
	(find-best-rhyme word
			 (cdr possibilities)
			 word-sounds sofar sofar-score))))))