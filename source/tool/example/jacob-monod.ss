;; http://www.scholarpedia.org/article/Predator-prey_model
(define-model jacob-monod
  (variable t :independent)
  (variable x)
  (variable y)
  (parameter V) ; the update velocity
  (parameter K) ; the saturation constant
  (parameter Y) ; the yield fo x per unit y taken up
  (eq (diff (bvar t) x)
      (times (divide (times V y) (plus K y)) x))
  (eq (diff (bvar t) y)
      (minus (times (divide 1 Y) (divide (times V y) (plus K y)) x))))
