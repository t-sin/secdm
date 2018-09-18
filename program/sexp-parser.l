;;;; S-expression parser

(ldc "> ")
(print)
(input)

(dum)

;; list parser `lisp`: ((sp . (skip . (symp . lisp))) . (ch . rest)) -> list
(ldf ((ldc "list parser") (println) (rtn)))

;; symbol parser `symp`: ((sp . (skip . (symp . lisp))) . (ch . rest)) -> symbol
(ldf ((ldc "symbol parser") (println) (rtn)))
(cons)

;; whitespace skipper `skip`: ((sp . (skip . (symp . lisp))) . (ch . rest)) -> nil
(ldf ((ldc "skip whitespaces") (println)
      (ld 0) (println)
      (ld 0) (cdr) (cdr) (ldc "") (eq)
      (sel ((ldc "") (join))
           ((ld 0) (cdr) (car) (ldc " ") (eq)
            (ld 0) (println)
            (join)))
      (rtn)))
(cons)

;; sexp parser `sp`: ((sp . (skip . (symp . lisp))) . (ch . rest)) -> sexp
(ldf ((ld 0) (cdr) (cdr) (ldc "") (eq)
      (sel (;; end of string
            (join))
           (;; skip whitespaces
            (ld 0) (cdr)
            (ld 0) (car) (cdr) (car)
            (cons)
            (ld 0) (car) (cdr) (car)
            (ap)
            ;; list
            (ld 0) (cdr) (car) (ldc "(") (eq)
            (sel ((ld 0) (cdr) (cdr) (ch)
                  (ld 0) (car)
                  (cons)
                  (ld 0) (car) (cdr) (cdr) (cdr)
                  (ap)
                  (join))
                 (;; otherwise symbol
                  (ld 0) (cdr) (cdr) (ch)
                  (ld 0) (car)
                  (cons)
                  (ld 0) (car) (cdr) (cdr) (car)
                  (ap)
                  (join)))
            (join)))
      (rtn)))
(cons)

(cons)

;; parser starter: ((sp . (skip . (symp . lisp))) . str) -> sexp
(ldf ((ld 0) (cdr) (ch) (ld 0) (car) (cons)
      (ld 0) (car) (car) (rap)
      (rtn)))
(ap)

(println)
(stop)