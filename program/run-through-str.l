;;;; run through the string

(ldc "input anything> ")
(print)
(input)
(ch)

(dum)
(ldf ((ld 0) (cdr) (cdr) (ldc "") (eq)
      (ld 0) (cdr) (car) (println)
      (sel ((join))
           ((ld 0) (cdr) (cdr) (ch)
            (ld 0) (car)
            (cons)
            (ld 0) (car)
            (ap)
            (join)))
      (rtn)))

(cons)

(ldf ((ld 0) (ld 0) (car) (rap) (rtn)))
(ap)

(stop)
