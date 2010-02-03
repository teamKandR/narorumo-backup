;; *** general-purpose haiku

(defstanza haiku-stanza
  :lines (5 7 5))

(defpoeticform haiku
  :stanzas (haiku-stanza))

;; *** arbitrary four-line ABAB rhyming poem.
(defstanza abab
  :lines ((8 a)
	  (10 b)
	  (8 a)
	  (10 b)))

(defpoeticform abababab
  :stanzas (abab
	    abab))


;; from wikipedia entry on Rhyme Schemes
;;  Petrarchan sonnet: "abba abba cde cde" or "abba abba cdc cdc".
;;  Shakespearean sonnet: "abab cdcd efef gg".
;;  Spenserian sonnet: "abab bcbc cdcd ee".

(defstanza petrarchan-one
  :lines ((10 a)
	  (10 b)
	  (10 b)
	  (10 a)))

(defstanza petrarchan-two
  :lines ((10 c)
	  (10 d)
	  (10 c)))

(defpoeticform petrarchan-sonnet
  :stanzas (petrarchan-one
	    petrarchan-one
	    petrarchan-two
	    petrarchan-two))


;; ** limericks, yay! **
(defstanza limerick-stanza
  :lines ((8 a)
	  (8 a)
	  (5 b)
	  (5 b)
	  (8 a)))

(defpoeticform limerick
  :stanzas (limerick-stanza))
  
(defpoeticform two-limericks
  :stanzas (limerick-stanza
	    limerick-stanza))
