(defparameter *ltre-files*
    '("ltre/ltms"      ;; LTMS
      "ltre/linter"    ;; Interface
      "ltre/ldata"     ;; Database
      "ltre/lrules"    ;; Rule system
      "ltre/unify"     ;; Unifier
      "ltre/funify"    ;; Open-coding of unification
      "ltre/cwa"       ;; Closed-world assumption mechanism
      "ltre/dds"))     ;; Dependency-directed search facility

(defun load-ltre ()
  (dolist (file *ltre-files*)
    (load file)))

(defun load-teb ()
  (load-ltre)

  (load "utils.lisp")
  (load "rhymes.lisp")
  (load "poeticforms.lisp")
  (load "pos.lisp")
  (load "diction.lisp")
  (load "generate.lisp")
  (load "grammar-rules.lisp")
  (load "pos.lisp")
  (load "poeticform-definitions.lisp")

  (format t "Loading up the sound dictionary...~%")
  (load-sound-dictionary)

  (format t "Loading up the POS dictionary...~%")
  (load-pos-dictionary)

  (format t "... and words in our diction for today...~%")
  (use-dict-file "wordlist.sexpr")

  "done!!")
