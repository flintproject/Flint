;; Example 4.4 in E. Allen, Modeling with Itô Stochastic Differential Equations. <https://doi.org/10.1007/978-1-4020-5953-7>
(define-model allen2007-example4.04
  (variable t :independent)
  (variable X :random :default 0)
  (variable W :Wiener)
  (eq (differential X)
      (plus (differential t)
            (times X (differential W))))
  )
