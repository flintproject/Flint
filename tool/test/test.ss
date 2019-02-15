#!r6rs
(import (xunit)
        (model lang)
        (model record)
        (model formula latex)
        (model formula mathml)
        (model formula simpl)
        (chezscheme))

(define (model-list)
  (let* ((m (path-parent (current-directory)))
         (s (directory-separator))
         (p (format "~a~aexample" m s)))
    (filter (lambda (x)
              (and (file-regular? x)
                   (string=? (path-extension x) "ss")))
            (map (lambda (x) (format "~a~a~a" p s x)) (directory-list p)))))

(for-each load (model-list))

;; Preliminaries

(assert-equal? '(t x alpha beta gamma delta omega) (map variable-name (model-variables duffing)))
(let ((es (model-equations duffing)))
  (assert-= 1 (length es))
  (assert-equal? '(plus (diff (bvar (degree 2) t) x) (times delta (diff (bvar t) x)) (times beta x) (times alpha (power x 3)))
                 (equation-lhs (car es)))
  (assert-equal? '(times gamma (cos (times omega t)))
                 (equation-rhs (car es))))

(assert-equal? '(t V W I) (map variable-name (model-variables fitzhugh-nagumo)))
(let ((es (model-equations fitzhugh-nagumo)))
  (assert-= 2 (length es))
  (assert-equal? '(diff (bvar t) V)
                 (equation-lhs (car es)))
  (assert-equal? '(plus (minus (minus V (divide (power V 3) 3)) W) I)
                 (equation-rhs (car es)))
  (assert-equal? '(diff (bvar t) W)
                 (equation-lhs (cadr es)))
  (assert-equal? '(times 0.08 (minus (plus V 0.7) (times 0.8 W)))
                 (equation-rhs (cadr es))))

(assert-equal? '(x y alpha n) (map variable-name (model-variables laguerre)))
(let ((es (model-equations laguerre)))
  (assert-= 1 (length es))
  (assert-equal? '(plus (times x (diff (bvar (degree 2) x) y)) (times (plus (minus alpha x) 1) (diff (bvar x) y)) (times n y))
                 (equation-lhs (car es)))
  (assert-equal? 0
                 (equation-rhs (car es))))

;; Simplifying formulae

(define (test-formula-simplify expected f)
  (assert-equal? expected (formula-simplify f)))

(test-formula-simplify '(plus a 5) '(plus (plus 1 (plus 0 (times))) a 3))
(test-formula-simplify 'a '(minus (minus 0 (divide a 1))))
(test-formula-simplify '(minus (plus a b d) c) '(minus (plus a b) (minus c d)))
(test-formula-simplify '(times 21/10 a) '(times a (divide 7 5) 3 (divide 1 2)))

;; Writing formulae in MathML format

(define (test-formula->mathml m expected)
  (for-each
   (lambda (e xy)
     (assert-string=? (car xy) (formula->mathml (equation-lhs e) "m" '()))
     (assert-string=? (cdr xy) (formula->mathml (equation-rhs e) "m" '())))
   (model-equations m)
   expected))

(test-formula->mathml
 abc
 '(("<m:apply><m:diff/><m:bvar><m:ci>t</m:ci></m:bvar><m:ci>x</m:ci></m:apply>" .
    "<m:apply><m:plus/><m:apply><m:times/><m:ci>A</m:ci><m:apply><m:sin/><m:ci>z</m:ci></m:apply></m:apply><m:apply><m:times/><m:ci>C</m:ci><m:apply><m:cos/><m:ci>y</m:ci></m:apply></m:apply></m:apply>")
   ("<m:apply><m:diff/><m:bvar><m:ci>t</m:ci></m:bvar><m:ci>y</m:ci></m:apply>" .
    "<m:apply><m:plus/><m:apply><m:times/><m:ci>B</m:ci><m:apply><m:sin/><m:ci>x</m:ci></m:apply></m:apply><m:apply><m:times/><m:ci>A</m:ci><m:apply><m:cos/><m:ci>z</m:ci></m:apply></m:apply></m:apply>")
   ("<m:apply><m:diff/><m:bvar><m:ci>t</m:ci></m:bvar><m:ci>z</m:ci></m:apply>" .
    "<m:apply><m:plus/><m:apply><m:times/><m:ci>C</m:ci><m:apply><m:sin/><m:ci>y</m:ci></m:apply></m:apply><m:apply><m:times/><m:ci>B</m:ci><m:apply><m:cos/><m:ci>x</m:ci></m:apply></m:apply></m:apply>")
   ))

(test-formula->mathml
 duffing
 '(("<m:apply><m:plus/><m:apply><m:diff/><m:bvar><m:degree><m:cn>2</m:cn></m:degree><m:ci>t</m:ci></m:bvar><m:ci>x</m:ci></m:apply><m:apply><m:times/><m:ci>delta</m:ci><m:apply><m:diff/><m:bvar><m:ci>t</m:ci></m:bvar><m:ci>x</m:ci></m:apply></m:apply><m:apply><m:times/><m:ci>beta</m:ci><m:ci>x</m:ci></m:apply><m:apply><m:times/><m:ci>alpha</m:ci><m:apply><m:power/><m:ci>x</m:ci><m:cn>3</m:cn></m:apply></m:apply></m:apply>" .
    "<m:apply><m:times/><m:ci>gamma</m:ci><m:apply><m:cos/><m:apply><m:times/><m:ci>omega</m:ci><m:ci>t</m:ci></m:apply></m:apply></m:apply>")
   ))

(test-formula->mathml
 efk1
 '(("<m:apply><m:plus/><m:apply><m:diff/><m:bvar><m:degree><m:cn>4</m:cn></m:degree><m:ci>t</m:ci></m:bvar><m:ci>u</m:ci></m:apply><m:apply><m:times/><m:ci>q</m:ci><m:apply><m:diff/><m:bvar><m:degree><m:cn>2</m:cn></m:degree><m:ci>t</m:ci></m:bvar><m:ci>u</m:ci></m:apply></m:apply><m:apply><m:power/><m:ci>u</m:ci><m:cn>3</m:cn></m:apply><m:apply><m:minus/><m:ci>u</m:ci></m:apply></m:apply>" .
    "<m:cn>0</m:cn>")
   ))

(assert-string=? "<cn type=\"rational\">7<sep/>3</cn>" (formula->mathml 7/3 #f '()))

;; Writing formulae in LaTeX format

(define (test-formula->latex m expected)
  (for-each
   (lambda (e xy)
     (assert-string=? (car xy) (formula->latex (equation-lhs e)))
     (assert-string=? (cdr xy) (formula->latex (equation-rhs e))))
   (model-equations m)
   expected))

(test-formula->latex
 abc
 '(("\\frac{\\operatorname{d}\\!x}{\\operatorname{d}\\!t}" .
    "A \\sin(z) + C \\cos(y)")
   ("\\frac{\\operatorname{d}\\!y}{\\operatorname{d}\\!t}" .
    "B \\sin(x) + A \\cos(z)")
   ("\\frac{\\operatorname{d}\\!z}{\\operatorname{d}\\!t}" .
    "C \\sin(y) + B \\cos(x)")
   ))

(test-formula->latex
 chua1
 '(("\\frac{\\operatorname{d}\\!x}{\\operatorname{d}\\!t}" .
    "a (z - \\mathit{a1} x^3 - \\mathit{a2} x^2 - b x)")
   ("\\frac{\\operatorname{d}\\!y}{\\operatorname{d}\\!t}" .
    "- z")
   ("\\frac{\\operatorname{d}\\!z}{\\operatorname{d}\\!t}" .
    "- \\mathit{b1} x + y + \\mathit{b2} z")
   ))

(test-formula->latex
 duffing
 '(("\\frac{\\operatorname{d}^{2}\\!x}{\\operatorname{d}\\!t^{2}} + \\delta \\frac{\\operatorname{d}\\!x}{\\operatorname{d}\\!t} + \\beta x + \\alpha x^3" .
    "\\gamma \\cos(\\omega t)")
   ))

(test-formula->latex
 efk1
 '(("\\frac{\\operatorname{d}^{4}\\!u}{\\operatorname{d}\\!t^{4}} + q \\frac{\\operatorname{d}^{2}\\!u}{\\operatorname{d}\\!t^{2}} + u^3 + (- u)" .
    "0")
   ))

(test-formula->latex
 fitzhugh-nagumo
 '(("\\frac{\\operatorname{d}\\!V}{\\operatorname{d}\\!t}" .
    "V - \\frac{V^3}{3} - W + I")
   ("\\frac{\\operatorname{d}\\!W}{\\operatorname{d}\\!t}" .
    "0.08 (V + 0.7 - 0.8 W)")
   ))

(test-formula->latex
 jacob-monod
 '(("\\frac{\\operatorname{d}\\!x}{\\operatorname{d}\\!t}" .
    "\\frac{V y}{K + y} x")
   ("\\frac{\\operatorname{d}\\!y}{\\operatorname{d}\\!t}" .
    "- \\frac{1}{Y} \\frac{V y}{K + y} x")
   ))

(test-formula->latex
 laguerre
 '(("x \\frac{\\operatorname{d}^{2}\\!y}{\\operatorname{d}\\!x^{2}} + (\\alpha - x + 1) \\frac{\\operatorname{d}\\!y}{\\operatorname{d}\\!x} + n y" .
    "0")
   ))

(test-formula->latex
 logistic
 '(("\\frac{\\operatorname{d}\\!x}{\\operatorname{d}\\!t}" .
    "x (1 - x)")
   ))

(test-formula->latex
 lotka-volterra
 '(("\\frac{\\operatorname{d}\\!x}{\\operatorname{d}\\!t}" .
    "(b - p y) x")
   ("\\frac{\\operatorname{d}\\!y}{\\operatorname{d}\\!t}" .
    "(r x - d) y")
   ))

(test-formula->latex
 roessler
 '(("\\frac{\\operatorname{d}\\!x}{\\operatorname{d}\\!t}" .
    "- y - z")
   ("\\frac{\\operatorname{d}\\!y}{\\operatorname{d}\\!t}" .
    "x + a y")
   ("\\frac{\\operatorname{d}\\!z}{\\operatorname{d}\\!t}" .
    "b + z (x - c)")
   ))

(test-formula->latex
 van-der-pol
 '(("\\frac{\\operatorname{d}^{2}\\!x}{\\operatorname{d}\\!t^{2}} - \\epsilon (1 - x^2) \\frac{\\operatorname{d}\\!x}{\\operatorname{d}\\!t} + x" .
    "0")
   ))

(assert-string=? "\\lvert \\exp(x) + 1 \\rvert" (formula->latex '(abs (plus (exp x) 1))))
(assert-string=? "\\operatorname{arccosh}(2^{\\lceil x \\rceil})" (formula->latex '(arccosh (power 2 (ceiling x)))))
(assert-string=? "2^{1 + \\lfloor \\det (x - y) \\rfloor}" (formula->latex '(power 2 (plus 1 (floor (determinant (minus x y)))))))
(assert-string=? "\\min(\\max(a, b, c), \\max(x, y))" (formula->latex '(min (max a b c) (max x y))))

(report)
