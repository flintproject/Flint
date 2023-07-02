(define-model hypergeometric
  (variable t :independent :real)
  (variable x :real :default 0.1)
  (parameter a :default 1)
  (parameter b :default 1)
  (parameter c :default 2)
  (eq (minus (plus
              (times (times x (minus 1 x)) (diff (bvar (degree 2) t) x))
              (times (minus c (times (plus a b 1) t)) (diff (bvar t) x)))
             (times a b x))
      0))
