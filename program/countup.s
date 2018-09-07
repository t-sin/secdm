;;;; WIP: count up numbers
;;;; oooh..???

;; takes `n`. if `n == 0`,
;; then decrement `n` and recursively apply `(ld 1)`,
;; otherwise just return.
(ldf ((ld 0) (write)
      (ld 0) (ldc 0) (eq)
      (sel ((join))
           ((ld 0) (ldc -1) (add)
            (rap)
            (join)))
      (rtn)))


;; starter part of recursion.
;; here, number of count is set to stack top.
(dum)
(ldf ((ldc 10)
      (dum)
      (ldf ((ld 0)
            (ld 1)
            (rap)
            (rtn)))
      (rap)
      (rtn)))

(ap)
(stop)
