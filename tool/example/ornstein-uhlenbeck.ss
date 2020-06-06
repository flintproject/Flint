;; Ornstein-Uhlenbeck process
(define-model ornstein-uhlenbeck
  (variable t :independent)
  (variable X :default 10)
  (variable W :Wiener)
  (parameter theta :default 1)
  (parameter sigma :default 1)
  (eq (differential X)
      (plus (times (minus (times theta X)) (differential t))
            (times sigma (differential W))))
  )
