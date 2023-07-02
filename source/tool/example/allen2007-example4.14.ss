;; Example 4.14 in E. Allen, Modeling with Itô Stochastic Differential Equations. <https://doi.org/10.1007/978-1-4020-5953-7>
(define-model allen2007-example4.14
  (variable t :independent)
  (variable X :random)
  (variable W :Wiener)
  (parameter a)
  (parameter b)
  (eq (differential X)
      (plus (times a (differential t))
            (times b (differential W))))
  )
