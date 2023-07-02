;; (4.5) and (4.6) in D. J. Higham, An Algorithmic Introduction to Numerical Simulation of Stochastic Differential Equations.
;; <https://doi.org/10.1137/S0036144500378302>
(define-model higham2001-4.5-4.6
  (variable t :independent)
  (variable W :Wiener)
  (variable X :random :default 'X_0)
  (variable Y)
  (parameter lambda :default 1)
  (parameter mu :default 1)
  (parameter X_0 :default 1)
  (eq (differential X)
      (plus (times (times lambda X) (differential t))
            (times (times mu X) (differential W))))
  (eq Y
      (times X_0
             (exp (plus (times (minus lambda (times 1/2 (power mu 2))) t)
                        (times mu W)))))
  )
