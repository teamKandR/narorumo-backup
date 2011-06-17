;; Let's model how an LTG match works!

#lang scheme
(require redex)

(provide ltg
         ltg-red)

(define-language ltg

  ;; Matches.
  (Match (YourSlots TheirSlots Expr Ticks Turns))
  (Ticks number) ;; initially 1000
  (Turns number) ;; initially 100000, meaning 100000 turns of *each*
                 ;; player
  
  ;; An LTG match comprises:
  ;;
  ;;   * The proponent's slots (yours).
  ;;   * The opponent's slots (theirs).
  ;;   * An expression.
  ;;   * A counter for how many computation steps remain in this turn.
  ;;   * A counter for how many turns remain.
  ;;
  ;; As each turn runs, the proponent's slots may change and the
  ;; opponent's slots may be side-effected.

  ;; Slots.
  (YourSlots Slots)
  (TheirSlots Slots)
  (Slots (Slot ...)) ;; 256 of them.
  (Slot (SlotNumber FieldValue Vitality))
  (SlotNumber number) ;; ranges from 0 to 255
  (FieldValue Function number) ;; number ranges from 0 to 65535
  (Vitality number) ;; ranges from -1 to 65535; initially 10000

  ;; Functions.
  (Function (lambda (Var) Expr))

  ;; Results.
  (Result (YourSlots TheirSlots Val Ticks Turns))
  ;; A Result is what you get when you're done with a match.

  ;; Expressions.
  (Expr (I Expr)
        zero
        (succ Integer)
        (dbl Integer)
        (get SlotNumber)
        (put Expr Expr)
        (S Expr Expr Expr)
        (K Expr Expr)
        (inc SlotNumber)
        (dec SlotNumber)
        (attack SlotNumber SlotNumber Integer)
        (help SlotNumber SlotNumber Integer)
        (copy SlotNumber)
        (revive SlotNumber)
        (zombie SlotNumber FieldValue)
        (Expr Expr)
        Var)

  ;; Evaluation contexts.
  (EvalCtxt hole
            (I EvalCtxt)
            (S EvalCtxt Expr Expr)
            (S Val EvalCtxt Expr)
            (S Val Val EvalCtxt)
            (K EvalCtxt Expr)
            (K Val EvalCtxt)
            (EvalCtxt Expr)
            (Val EvalCtxt))

  (Instr Hval
         (Val Val)
         (I Val)
         (S Val Val Val)
         (K Val Val))
  ;; An Instr is something that we have to define the ---> reduction
  ;; relation on.  Expressions have to keep being evaluated using the
  ;; standard --> reduction semantics, but Instrs mean that we either
  ;; have to write something in a slot, look something up in a slot,
  ;; or maybe both.

  ;; Integer constants.
  (Integer number) ;; greater than or equal to 0

  ;; Any symbol not mentioned as a literal in the grammar of the
  ;; language is fair game to be a variable.
  (Var variable-not-otherwise-mentioned))

(define ltg-red
  (reduction-relation
   ltg

   ;; Program -> Program/Result

   ;; Identity function.  No side effects except one fewer tick
   (---> (YourSlots TheirSlots (in-hole EvalCtxt (I Val)) Ticks Turns)
         (YourSlots TheirSlots (in-hole EvalCtxt Val)
                    `(- ,(term Ticks) 1) Turns)
         "I")

   (---> (YourSlots TheirSlots (in-hole EvalCtxt (S Val_1 Val_2 Val_3))
                    Ticks Turns)
         (YourSlots TheirSlots (in-hole EvalCtxt ((Val_1 Val_3) (Val_2 Val_3)))
                    `(- ,(term Ticks) 1) Turns)
         "S")

   (---> (YourSlots TheirSlots (in-hole EvalCtxt (K Val_1 Val_2)) Ticks Turns)
         (YourSlots TheirSlots (in-hole EvalCtxt Val_1)
                    `(- ,(term Ticks) 1) Turns)
         "K")

   (---> (YourSlots TheirSlots (in-hole EvalCtxt (Card Slot)) Ticks Turns)
         ((replace-slot Slot I YourSlots) TheirSlots
                    (in-hole EvalCtxt (Card (slot-lookup YourSlots Slot)))
                    `(- ,(term Ticks) 1) Turns)
         "LeftApp")

   (---> (YourSlots TheirSlots (in-hole EvalCtxt (Card Slot)) Ticks Turns)
         ((replace-slot Slot I YourSlots) TheirSlots
                    (in-hole EvalCtxt (Card (slot-lookup YourSlots Slot)))
                    `(- ,(term Ticks) 1) Turns)
         "RightApp")

   with
   [(--> (in-hole EvalCtxt a) (in-hole EvalCtxt b)) (---> a b)]))

(define-metafunction baby-rust
  slot-lookup : ???
  [(slot-lookup Slots Slot) ???])

(define-metafunction baby-rust
  replace-slot : ???
  [(replace-slot Slot Function Slots) ???])

