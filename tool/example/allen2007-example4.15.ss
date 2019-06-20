;; Example 4.15 in E. Allen, Modeling with Itô Stochastic Differential Equations. <https://doi.org/10.1007/978-1-4020-5953-7>
(define-model allen2007-example4.15
  (variable t :independent)
  (variable X :random :default 18)
  (variable W :Wiener)
  (parameter theta_1 :default 0.0361)
  (parameter theta_2 :default 0.609)
  (eq (differential X)
      (plus (times theta_1 X (differential t))
            (times (root (times theta_2 X)) (differential W))))
  )
