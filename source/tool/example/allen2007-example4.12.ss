;; Example 4.12 in E. Allen, Modeling with Itô Stochastic Differential Equations. <https://doi.org/10.1007/978-1-4020-5953-7>
(define-model allen2007-example4.12
  (variable t :independent)
  (variable X :random :default 1)
  (variable W_1 :Wiener)
  (variable W_2 :Wiener)
  (eq (differential X)
      (plus (times (power t 2) X (differential t))
            (times t (differential W_1))
            (times X (differential W_2))))
  )
