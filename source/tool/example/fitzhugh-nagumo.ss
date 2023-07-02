(define-model fitzhugh-nagumo
  (variable t :independent :real)
  (variable V :real)
  (variable W :real)
  (parameter I :real)
  (eq (diff (bvar t) V)
      (plus (minus (minus V
                          (divide (power V 3) 3))
                   W)
            I))
  (eq (diff (bvar t) W)
      (times 0.08
             (minus (plus V 0.7)
                    (times 0.8 W)))))
