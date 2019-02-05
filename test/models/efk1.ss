;; Global dynamics of stationary solutions of the extended Fisher–Kolmogorov equation
;; Journal of Mathematical Physics 52, 112701 (2011); https://doi.org/10.1063/1.3657425
(define-model efk1
  (variable t :independent)
  (variable u)
  (parameter q (< q 0))
  (eq (plus (diff (bvar (degree 4) t) u)
            (times q (diff (bvar (degree 2) t) u))
            (power u 3)
            (minus u))
      0))
