;; fibonacci

(ldc 11)
(ldf ((ld 0)
      (ldc 1)
      (eq)
      (sel ((ldc 1) (join))
           ((ldc "hoge") (write) (join)))
      (rtn)))
(ap)
(stop)
