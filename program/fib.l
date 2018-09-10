;;;; calculate fibonacci number

(dum)

(ldc 0)   ; counter 
(ldc 20)  ; number of max n
(cons)

;; fib-fn: (fib-fn . n) => F_n
(ldf ((ld 0) (cdr) (ldc 0) (eq) ;; F_0
      (sel ((ldc 0) (join))
           ((ld 0) (cdr) (ldc 1) (eq) ;; F_1
            (sel ((ldc 1) (join))
                 ((ld 0) (cdr) (ldc -2) (add) (ld 0) (car) (cons) (ld 0) (car) (ap)
                  (ld 0) (cdr) (ldc -1) (add) (ld 0) (car) (cons) (ld 0) (car) (ap)
                  (add)
                  (join)))
            (join)))
      (rtn)))
(cons)

;; rec-fn: (rec-fn . (fib-fn . (max . n))) -> _
(ldf ((ld 0) (cdr) (cdr) (cdr) (ld 0) (cdr) (cdr) (car) (eq)
      (sel ((join))
           (;; call fib-fn
            (ld 0) (cdr) (cdr) (cdr)
            (ld 0) (cdr) (car)
            (cons)
            (ld 0) (cdr) (car)
            (ap) (println)
            ;; call rec-fn
            (ld 0) (cdr) (cdr) (cdr) (ldc 1) (add)
            (ld 0) (cdr) (cdr) (car)
            (cons)
            (ld 0) (cdr) (car)
            (cons)
            (ld 0) (car)
            (cons)
            (ld 0) (car)
            (ap)
            (join)))
      (rtn)))
(cons)

(ldf ((ld 0) (ld 0) (car) (rap)
      (rtn)))

(ap)
(stop)
