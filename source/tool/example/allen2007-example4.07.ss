;; Example 4.7 in E. Allen, Modeling with Itô Stochastic Differential Equations. <https://doi.org/10.1007/978-1-4020-5953-7>
(define-model allen2007-example4.07
  (variable t :independent)
  (variable X :random)
  (variable W :Wiener)
  (parameter alpha)
  (parameter sigma)
  (eq (differential X)
      (plus (minus (times alpha (differential t)))
            (times sigma (differential W))))
  )
