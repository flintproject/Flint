(define-model van-der-pol
  (variable t :independent :real)
  (variable x :real)
  (parameter epsilon :real
             (< epsilon 0))
  (eq (plus (minus (diff (bvar (degree 2) t) x)
                   (times epsilon
                          (minus 1 (power x 2))
                          (diff (bvar t) x)))
            x)
      0))
