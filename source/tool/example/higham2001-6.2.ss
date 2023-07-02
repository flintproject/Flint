;; (6.2) in D. J. Higham, An Algorithmic Introduction to Numerical Simulation of Stochastic Differential Equations.
;; <https://doi.org/10.1137/S0036144500378302>
(define-model higham2001-6.2
  (variable t :independent)
  (variable W :Wiener)
  (variable X :random :default 'X_0)
  (parameter X_0 :default 0.5)
  (parameter r :default 2)
  (parameter K :default 1)
  (parameter beta :default 0.25)
  (eq (differential X)
      (plus (times (times r (times X (minus K X))) (differential t))
            (times (times beta X) (differential W))))
  )
