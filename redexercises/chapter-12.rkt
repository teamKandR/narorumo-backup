#lang racket
(require redex)

;; Exercises from Part II of the PLT Redex book.

;; Chapter 12: Variables and Meta-functions

;; 12.1

;; The grammar of Error ISWIM in Redex.

(define-language error-iswim
  ;; Expressions.  The list (M N L K) means that any of those
  ;; nonterminals may be used for error-iswim expressions in the rest
  ;; of the language definition.
  ((M N L K) X (lambda X M) (M M) b (o2 M M) (o1 M) (err Lbl))
  ;; Primops
  (o o1 o2)
  ;; Unary primops
  (o1 add1 sub1 iszero)
  ;; Binary primops
  (o2 + - * ^)
  ;; Numbers
  (b number)
  ;; Values
  ((V U W) b X (lambda X M))
  ;; Evaluation contexts
  (E hole (V E) (E M) (o V ... E M ...))
  ;; Variables.  Here, similarly, we can use X or Y or Z for variables
  ;; in the rest of the language definition.
  ((X Y Z) variable-not-otherwise-mentioned)
  ;; Errors
  (Lbl label0 label1 label2))

;; Um, I think that's all there is to it?  Just added an (err Lbl)
;; construct to the core language.  I don't know if there's a way to
;; talk about an infinite set of labels in Redex.

;; 12.2

;; The grammar of State ISWIM in Redex.

(define-language state-iswim
  ;; Expressions
  ((M N L K) X (lambda X M) (M M) b (o2 M M) (o1 M) (set X M))
  ;; Primops
  (o o1 o2)
  ;; Unary primops
  (o1 add1 sub1 iszero)
  ;; Binary primops
  (o2 + - * ^)
  ;; Numbers
  (b number)
  ;; Values
  ((V U W) b X (lambda X M))
  ;; Evaluation contexts
  (E hole (V E) (E M) (o V ... E M ...))
  ;; Variables
  ((X Y Z) variable-not-otherwise-mentioned))

;; ...again, not much of interest here.  All the interestingness of
;; errors and state is in the semantics, not the syntax.


