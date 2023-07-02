;; https://www.encyclopediaofmath.org/index.php/Laguerre_functions
(define-model laguerre
  (variable x :independent :real)
  (variable y :real)
  (parameter alpha :real)
  (parameter n :natural)
  (eq (plus (times x (diff (bvar (degree 2) x) y))
            (times (plus (minus alpha x) 1)
                   (diff (bvar x) y))
            (times n y))
      0))
