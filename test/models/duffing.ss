(define-model duffing
  (variable t :independent :real)
  (variable x :real)
  (parameter alpha :real :default -1)
  (parameter beta :real :default 1)
  (parameter gamma :real :default 0.5)
  (parameter delta :real :default 0.3)
  (parameter omega :real :default 1.2)
  (eq (plus (diff (bvar (degree 2) t) x)
            (times delta (diff (bvar t) x))
            (times beta x)
            (times alpha (power x 3)))
      (times gamma (cos (times omega t)))))