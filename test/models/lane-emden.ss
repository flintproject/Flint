;; http://mathworld.wolfram.com/Lane-EmdenDifferentialEquation.html
(define-model lane-emden
  (variable xi :independent)
  (variable theta)
  (parameter n :natural)
  (eq (plus (diff (bvar (degree 2) xi) theta)
            (times (divide 2 xi) (diff (bvar xi) theta))
            (power theta n))
      0))
