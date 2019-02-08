;; http://www.scholarpedia.org/article/Predator-prey_model
(define-model lotka-volterra
  (variable t :real :independent)
  (variable x)
  (variable y)
  (parameter b) ; the growth rate of the prey x
  (parameter p) ; the impact of predation
  (parameter d) ; the death rate of the predator y
  (parameter r) ; the growth rate of the predator y
  (eq (diff (bvar t) x)
      (times (minus b (times p y)) x))
  (eq (diff (bvar t) y)
      (times (minus (times r x) d) y)))
