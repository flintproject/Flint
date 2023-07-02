;; Example 4.5 in E. Allen, Modeling with Itô Stochastic Differential Equations. <https://doi.org/10.1007/978-1-4020-5953-7>
(define-model allen2007-example4.05
  (variable t :independent)
  (variable X :random :default 1/2)
  (variable W :Wiener)
  (eq (differential X)
      (plus (minus (times 1/4 (power X 3) (differential t)))
            (times 1/2 (power X 2) (differential W))))
  )
