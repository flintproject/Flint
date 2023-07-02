;; Example 4.6 in E. Allen, Modeling with Itô Stochastic Differential Equations. <https://doi.org/10.1007/978-1-4020-5953-7>
(define-model allen2007-example4.06
  (variable t :independent)
  (variable X :random :default 1)
  (variable W :Wiener)
  (eq (differential X)
      (plus (times (plus (times 1/3 (power X 1/3))
                         (times 6 (power X 2/3)))
                   (differential t))
            (times (power X 2/3) (differential W))))
  )
