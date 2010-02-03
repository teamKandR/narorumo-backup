(defun install-grammar (gram)
  (loop for rule in gram do

	(cond
	 ((null (cdr rule))
	  (rassert! (:eval `(terminal ,(car rule)))))
	  
	 (t
	  (rassert! (:eval `(can-be  ,(car rule) ,(cdr rule))))))))

(defun possible-nexts (current sofars)
  (assuming `( (working-on ,current)
	      (so-far ,sofars))
	    
	    (run-rules)
	    
	    (loop for node in (fetch `(possible-next ?foo))
		  when (true? node)
		  collect (second node))))

(defun terminal? (sym)
  (not (null (fetch `(terminal ,sym)))))

(defun nonterminal? (sym)
  (not (null (fetch `(can-be ,sym ?canbe)))))

(defun pick-shortest-possible (possibles sofar len-sofar)
  (cond
   ((null possibles) sofar)

   (t
    (let* ((expansion (shortest-expansion (car possibles)))
	   (len (length expansion)))

      (format t "expansion of ~a: ~a~%" (car possibles) expansion)

      (if (< len len-sofar)
	  (pick-shortest-possible (cdr possibles) (car possibles) len)
	(pick-shortest-possible (cdr possibles) sofar len-sofar))))))

(defun pick-possible (possibles &optional shortest?)
  (if shortest?
      (pick-shortest-possible possibles nil most-positive-fixnum)
    (pick-one possibles)))

(defun generate (target &optional (sofar nil) (shortest? nil))
  (let ((possibles (possible-nexts target sofar)))

    (cond
     ((null possibles) sofar)
     (t (generate target
		  (append sofar (list
				 (pick-possible possibles shortest?))))))))

(defun expand-to-poses (syms nbeats)
  (cond
   ((null syms) nil)

   ((and nbeats (> (min-length-total syms) nbeats))
    :backup)

   ((terminal? (car syms))
    (let ((restbit (expand-to-poses (cdr syms) (and nbeats
						    (1- nbeats)))))
      (if (eq :backup restbit)
	  (progn ;; (format t "we think it's possible. try again.~%")
		 (expand-to-poses syms nbeats))
	(cons (car syms)
	      restbit))))
   
   ((nonterminal? (car syms))
     (let ((firstbit (expand-to-poses (generate (car syms) nil)
				      nbeats)))
       (if (eq :backup firstbit)
	   (progn ;; (format t "we think it's possible. try again.~%")
		  (expand-to-poses syms nbeats))
	 (let ((restbit (expand-to-poses
			(cdr syms)
			(and nbeats
			     (- nbeats (length firstbit))))))
	   (if (eq :backup restbit)
	       (progn ;; (format t "we think it's possible. try again.~%")
		      (expand-to-poses syms nbeats))
	     (append firstbit restbit))))))))

(defun shortest-expansion (sym)
  (expand-to-poses (list sym) most-positive-fixnum t))

 
(defun choose-words-map (poses)
  (mapcar #'pos-to-word poses))


(defun build-line (&key rhymes-with butnot (nbeats nil))
  (let ((poses (expand-to-poses '(line) nbeats)))
    (cond
     ((eq :backup poses)
      'unsatisfiable?)

     (t
      ;; (format t "Trying these out: ~a.~%" poses)
      (let ((possible (choose-words poses
				    :nbeats nbeats
				    :rhymes-with rhymes-with
				    :butnot butnot)))

	(if (member nil possible)
	    (build-line :rhymes-with rhymes-with
			:butnot butnot
			:nbeats nbeats)
	  possible))))))

(defun build-line-front (linedesc rhymeenv)
  (cond
   ((listp linedesc)

    (let ((rt (get-rhymetag rhymeenv (second linedesc))))
      (build-line :rhymes-with (car (rhymetag-words rt))
		  :butnot (rhymeenv-words rhymeenv)
		  :nbeats (car linedesc))))

   ((atom linedesc)
    (build-line :nbeats linedesc))))
   
;; stanzas are lists of lines. lines are lists of words.
;; poems are lists of stanzas. hut.
(defun build-stanza (stype &key rhymeenv)
  (let ((st (if (stanzatype-p stype) stype
	      (get-stanzatype stype))))
    
    (loop for line in (stanzatype-lines st)
	  collect (let ((nextline (build-line-front line rhymeenv)))

		    (add-to-rhymeenv rhymeenv
				     (car (last nextline))
				     line)
		    nextline))))


(defun build-poem (poeticform)
  (let ((pf (if (poeticform-p poeticform) poeticform
	      (get-poeticform poeticform))))
    (cond
     ((null pf)
      (format t "[build-poem] No such poetic form!~%")
      nil)

     (t 
      (terpri)
      (let ((renv (new-rhyme-environment pf)))

	(loop for s in (poeticform-stanzas pf)
	      collect (build-stanza s :rhymeenv renv)))))))