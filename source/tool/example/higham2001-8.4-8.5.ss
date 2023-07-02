;; (8.4) and (8.5) in D. J. Higham, An Algorithmic Introduction to Numerical Simulation of Stochastic Differential Equations.
;; <https://doi.org/10.1137/S0036144500378302>
(define-model higham2001-8.4-8.5
  (variable t :independent)
  (variable W :Wiener)
  (variable X :random :default 'X_0)
  (variable V :random :default '(root X_0))
  (variable sqrtX)
  (parameter X_0 :default 1)
  (parameter alpha :default 2)
  (parameter beta :default 1)
  (eq (differential X)
      (plus (times (minus alpha X) (differential t))
            (times (times beta (root X)) (differential W))))
  (eq (differential V)
      (plus (times (minus (divide (minus (times 4 alpha) (power beta 2)) (times 8 V)) (times 1/2 V)) (differential t))
            (times (times 1/2 beta) (differential W))))
  (eq sqrtX (root X))
  )
