;; The bump function <https://en.wikipedia.org/wiki/Bump_function>
(define-model bump
  (variable t :real :independent)
  (variable b :real :default '(exp -1))
  (eq (diff (bvar t) b)
      (piecewise
       (piece (minus (times (divide (times 2 t) (power (minus 1 (power t 2)) 2)) b))
              (lt (abs t) 1))
       (otherwise 0))))
