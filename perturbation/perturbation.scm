;; Perturbations for Scheme!
;; Runs with a recent version of mzscheme or DrScheme that supports the
;; (require srfi/n) syntax.

;; From the commandline:
;; $ mzscheme -f ./perturbation.scm -e "(print-perturbations)"

(require srfi/1)  ;; lists
(require srfi/13) ;; strings
(require srfi/69) ;; hash tables

;; Get a list of all the words in a given dictionary file.
(define (list-words fn)  
  (define (accumulate-words sofar)
    (let ((line (read-line)))
      (if (eof-object? line)
          sofar
          (accumulate-words (cons line sofar)))))
  (define (words-helper)
    (accumulate-words '()))  
  (reverse (with-input-from-file fn words-helper)))

;; A lookup key is a list of characters. Get the lookup key for a given word.
(define (word->key word)
  (sort (string->list word) char<?))

;; Take a list of words and return pairs of the form (word . key) for all
;; the words that could be in the target space (ie, have an #\r and a #\b).
(define (words->pairs words)
  (define (maybe-pair word)
    (if (and (string-contains word "r") (string-contains word "b"))
        (cons (word->key word) word)
        #f))
  (filter-map maybe-pair words))

;; Build the hash table that maps from keys to the apropos list of words.
(define (pairs->table pairs)
  (letrec ((out (make-hash-table))
           (add-pairs
            (lambda (pair)
              (when (not (hash-table-exists? out (car pair)))
                (hash-table-set! out (car pair) null))
              (hash-table-set! out
                               (car pair)
                               (cons (cdr pair)
                                     (hash-table-ref out (car pair)))))))
    (for-each add-pairs pairs)
    out))

;; List all the perturbations for a given word, given the key->words table.
(define (get-perturbations word table)
  (let ((mindex (string-contains word "m")))
    (and mindex
         (let ((key (word->key (string-append
                                (string-take word mindex)
                                (string-drop word (+ 1 mindex))
                                "rb"))))
           (and (hash-table-exists? table key)
                (hash-table-ref table key))))))

;; Print out all the perturbations in your dictionary.
(define (print-perturbations)
  (letrec ((words (list-words "/usr/share/dict/words"))
           (table (pairs->table (words->pairs words))))   
    (for-each
     (lambda (word)
       (let ((perturbations (get-perturbations word table)))
         (when perturbations
           (begin (newline)
                  (display (string-append word ": "))
                  (display perturbations)))))
     words)))
