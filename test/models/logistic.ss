(define-model logistic
  (variable t :independent :real)
  (variable x :real)
  (eq (diff (bvar t) x)
      (times x (minus 1 x))))
