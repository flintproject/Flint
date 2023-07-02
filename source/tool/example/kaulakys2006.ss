;; Equation (9) and (10) of:
;; Kaulakys, B., Ruseckas, J., Gontis, V. & Alaburda, M. Nonlinear stochastic models of 1/f noise and power-law distributions. Physica A: Statistical Mechanics and its Applications 365, 217–221 (2006).
(define-model kaulakys2006
  (variable t :independent :real)
  (variable X :real :default 1)
  (variable W :Wiener)
  (parameter mu :real :default 0)
  (parameter sigma :real :default 1)
  (parameter gamma :real :default 1/2)
  (parameter eta :real :default '(minus 5/2 mu))
  (parameter Gamma :real :default '(minus 1 (divide gamma (power sigma 2))))
  (eq (differential X)
      (plus (times Gamma
                   (power X (minus (times 2 eta) 1))
                   (differential t))
            (times (power X eta)
                   (differential W)))))
