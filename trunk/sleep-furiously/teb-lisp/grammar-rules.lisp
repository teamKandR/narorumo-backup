
(in-ltre (create-ltre 'teb))

(defvar *grammar*
  '((line pp)
    (line np)
    (line s)
    
    (s  s conj s)
    (s  np vp2 conj vp2)
    (s  np vp1)
    ;; (s  np vp2)
    (s !)
    
    (pp prep np)
    (s  np vp1)
    (s  np vp2)
    (np  art n)
    (np  art adj n)
    (np  art adj adj n)
    (np  pron)
    (vp1 trans np)
    (vp2 intrans)

    ;; terminals
    (!)
    (intrans)
    (trans)
    (n)
    (art)
    (adj)
    (prep)
    (conj)
    (pron)))

(install-grammar *grammar*)


(defun lookup-min-length (sym)
  (if (nonterminal? sym)
      (let ((fetched (fetch `(possible-length ,sym ?len))))
	(if fetched
	    (loop for candidate in fetched
		  minimizing (third candidate))
	  nil))
    1))

(defun all-lengths-known (syms)
  (loop for sym in syms
	always (let ((fetched (fetch `(min-length ,sym ?len))))
		 (and fetched (true? (car fetched))))))

(defun sum-lengths (syms)
  (loop for sym in syms
	sum (let ((fetched (fetch `(min-length ,sym ?len))))
		 (if (and fetched (true? (car fetched)))
		     (third (car fetched))
		   (progn (format t "[sum-lengths] Eep! It wasn't true.~%")
			  0)))))



(defun record-min-length (sym len)
	(rassert! (:eval `(min-length ,sym ,len))))


(defun known-min-length (sym)
  (let ((fetched (fetch `(min-length ,sym ?len))))
    (if fetched
	(third (car fetched))
      nil)))

(defun min-length-nonterm (sym &optional seen)
  (or (known-min-length sym)
      (calculate-min-length sym seen)))


(defun calculate-min-length (sym &optional seen)
  (let* ((productions (fetch `(can-be ,sym ?things)))
	 (min-len
	  (loop for prod in productions
		minimize
		(loop for rhs-elt in (third prod)
		      sum
		      (if (member rhs-elt seen)
			  10000
			(min-length
			 rhs-elt
			 (cons rhs-elt seen)  ))))))

    (record-min-length sym min-len)
    min-len))


(defun min-length (sym &optional seen)
  (cond 
   ((terminal? sym) 1)
   
   ((nonterminal? sym)
    (min-length-nonterm sym seen))))

(defun min-length-total (syms)
  (loop for sym in syms
	summing (min-length sym)))

(rule ((:true (can-be ?target ?things) :var ?v1))
      (loop for i from 0 below (length ?things) do
	    (progn
	      (rassert!
	       (:implies (:and (:eval `(working-on ,?target))
			       (:eval `(so-far ,(subseq ?things 0 i))))
			 (:eval `(possible-next  ,(elt ?things i))))))))

(rule ((:true (terminal ?sym) :var ?v1))
      (rassert! (min-length ?sym 1)))

(rule ((:true (can-be ?sym ?things) :var ?v1
	      :test (all-lengths-known ?things)))
      (rassert! (possible-length ?sym (:eval (sum-lengths ?things)))))

(run-rules)

(rule ((:true (possible-length ?sym ?len1) :var ?v1)
       (:true (can-be ?sym ?things) :var ?v2)
       (:true (possible-length ?sym ?len2) :var ?v3
	      :test (< ?len1 ?len2)))

      (record-min-length ?sym))

(run-rules)
