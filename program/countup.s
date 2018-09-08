;;;; count up numbers

;; push dummy value into current environment.
(dum)

;; push a counter on the stack top
(ldc 0)

;; defines the recursive function `rf`:
;; this applies `rf` in the cons, when cdr of arg is equals to the limit number `10`.
(ldf ((ld 0) (cdr) (ldc 10) (eq)
      (sel ((ldc "stop counting up.") (write) (join))
           ((ld 0) (cdr) (write)        ; print current counter
            (ld 0) (cdr) (ldc 1) (add)  ; increment new number
            (ld 0) (car)                ; take `rf` from cons cel
            (cons)                      ; combine new number and `rf`
            (ld 0) (car) (ap)           ; apply `rf` with new cell
            (join)))
      (rtn)))

;; stores a limit number and the recursion function to a cons cell.
(cons)

;; the starter function `sf`
(ldf ((ld 0) (ld 0) (car) (rap)
      (rtn)))

(ap)
(stop)
