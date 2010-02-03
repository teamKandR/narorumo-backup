(defvar *poeticforms*
  nil
  "List of all known poetic forms")

(defvar *stanzatypes*
  nil
  "List of all known stanza types")

(defun poeticform-print-procedure (form st ignore)
  (declare (ignore ignore))
  (format st "[poeticform ~a]" (poeticform-name form)))

(defun stanzatype-print-procedure (form st ignore)
  (declare (ignore ignore))
  (format st "[stanza type ~a]" (stanzatype-name form)))


(defun rhymeenv-print-procedure (form st ignore)
  (declare (ignore ignore))
  (format st "[rhyme-env ~a: ~a]"
	  (rhyme-env-tags form)))


(defun rhymetag-print-procedure (form st ignore)
  (declare (ignore ignore))
  (format st "[rhymetag ~a: ~a]"
	  (rhyme-environment-label form)
	  (rhyme-environment-words form)))

(defstruct (poeticform (:print-function poeticform-print-procedure))
  stanzas name)

(defstruct (stanzatype (:print-function stanzatype-print-procedure))
  lines name)

(defstruct (rhymetag (:print-function rhymetag-print-procedure))
  label words)

(defstruct (rhyme-env (:print-function rhymenv-print-procedure))
  tags)

(defmacro defpoeticform (name &key stanzas)
  `(let ((newform (make-poeticform :name ',name
				   :stanzas ',stanzas)))
       
       (push newform *poeticforms*)
       (format t "Added ~a." ',name)))

(defmacro defstanza (name &key lines)
  `(let ((newstanza (make-stanzatype :name ',name
				     :lines ',lines)))
       (push newstanza *stanzatypes*)
       (format t "Added ~a." ',name)))

(defun get-stanzatype (name)
  (car (member-if #'(lambda(x) (eq name (stanzatype-name x)))
		  *stanzatypes*)))

(defun get-poeticform (name)
  (car (member-if #'(lambda(x) (eq name (poeticform-name x)))
		  *poeticforms*)))

(defun new-rhyme-environment (pf)
  (let* ((stanzatypes (mapcar #'get-stanzatype (poeticform-stanzas pf)))
	 (labs (loop for st in stanzatypes
		       append (loop for line in (stanzatype-lines st)
				    when (listp line)
				    collect (second line)))))
    (make-rhyme-env
     :tags (mapcar #'(lambda(lab)
		       (make-rhymetag :label lab))
		   (remove-duplicates labs)))))

(defun get-rhymetag (renv label)
  (car (member-if #'(lambda(x) (eq label (rhymetag-label x)))
		  (rhyme-env-tags renv))))


(defun add-to-rhymeenv (renv word linedesc)
  (cond
   ((atom linedesc) nil)

   ((listp linedesc)

    (let ((rt (get-rhymetag renv (second linedesc))))
      (push word (rhymetag-words rt))))))

(defun rhymeenv-words (renv)
  (loop for tag in (rhyme-env-tags renv)
	append (rhymetag-words tag)))