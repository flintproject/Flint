;; http://www.scholarpedia.org/article/Predator-prey_model
(define-model jacob-monod
  (variable t :independent)
  (variable x)
  (variable y)
  (parameter V) ; the update velocity
  (parameter K) ; the saturation constant
  (parameter Y) ; the yield fo x per unit y taken up
  (eq (diff (bvar t) x)
      (times (divides (times V y) (plus K y)) x))
  (eq (diff (bvar t) y)
      (minus (times (divides 1 Y) (divides (times V y) (plus K y)) x))))
