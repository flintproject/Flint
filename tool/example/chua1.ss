;; Analytic integrability of a Chua system
;; Journal of Mathematical Physics 49, 102701 (2008); https://doi.org/10.1063/1.2992481
(define-model chua1
  (variable t :independent)
  (variable x)
  (variable y)
  (variable z)
  (parameter a)
  (parameter b)
  (parameter a1 :default 44/3)
  (parameter a2 :default 41/2)
  (parameter b1 :default 7/10)
  (parameter b2 :default 6/25)
  (eq (diff (bvar t) x)
      (times a
             (minus z
                    (times a1 (power x 3))
                    (times a2 (power x 2))
                    (times b x))))
  (eq (diff (bvar t) y)
      (minus z))
  (eq (diff (bvar t) z)
      (plus (minus (times b1 x))
            y
            (times b2 z))))
