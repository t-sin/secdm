;;;; infinite loop

;; push dummy value into current environment.
(dum)

;; define the recursive function `rf`: it takes itsself, and call itsself with args; itsself.
;; function defined is pushed into stack top.
;; function definition copies current envrionment
(ldf ((ldc "looping...") (println)
      (ld 0) (ld 0) (ap)
      (rtn)))

;; starter function `sf`:
;; new envrionment is created and it stores the `rf` because the function
;; is argument of starter function.
;; the starter function applies `rf` with one args `rf`.
(ldf ((ld 0) (ld 0) (rap)
      (rtn)))

;; apply the starter function `sf`
(ap)

;; stop machine
(stop)
