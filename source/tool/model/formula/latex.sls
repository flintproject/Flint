#!r6rs
(library (model formula latex)
  (export formula->latex)
  (import (rnrs (6)))

  (define greek-symbols
    '(alpha
      beta
      gamma
      Gamma
      delta
      Delta
      epsilon
      zeta
      eta
      theta
      Theta
      iota
      kappa
      lambda
      Lambda
      mu
      nu
      xi
      Xi
      pi
      Pi
      rho
      sigma
      Sigma
      tau
      upsilon
      Upsilon
      phi
      Phi
      chi
      psi
      Psi
      omega
      Omega))

  (define (greek-prefix? s)
    (assert (string? s))
    (exists
     (lambda (g)
       (and (< (string-length g) (string-length s))
            (string=? g (substring s 0 (string-length g)))
            (char=? #\_ (list-ref (string->list s) (string-length g)))))
     (map symbol->string greek-symbols)))

  ;; level
  ;;   atom: number, indentifier, etc.
  ;;   factor: function call, power, root, logarithm, etc.
  ;;   product: fraction
  ;;   sum: difference
  ;;   statement
  (define-record-type tree
    (fields level
            body))

  (define (make-atom b)
    (make-tree 'atom b))

  (define (make-factor b)
    (make-tree 'factor b))

  (define (make-product b)
    (make-tree 'product b))

  (define (make-sum b)
    (make-tree 'sum b))

  (define (make-statement b)
    (make-tree 'statement b))

  (define (tree->atom t)
    (case (tree-level t)
      ((atom) t)
      (else (make-atom (list "{" t "}")))))

  (define (tree->product t)
    (case (tree-level t)
      ((atom factor product) t)
      (else (make-product (list "(" t ")")))))

  (define (tree->sum t)
    (case (tree-level t)
      ((atom factor product sum) t)
      (else (make-sum (list "(" t ")")))))

  (define (formula->tree f)
    (assert (not (null? f)))
    (cond ((symbol? f)
           (cond ((= (string-length (symbol->string f)) 1)
                  (make-atom f))
                 ((memq f greek-symbols)
                  (make-atom (list "\\" f)))
                 ((greek-prefix? (symbol->string f))
                  (make-atom (list "\\" f)))
                 (else
                  (make-atom (list "\\mathit{" f "}")))))
          ((number? f)
           (if (and (exact? f)
                    (rational? f)
                    (not (integer? f)))
               (let* ((n (numerator f))
                      (d (denominator f))
                      (x (list "\\frac{" (abs n) "}{" d "}")))
                 (if (negative? n)
                     (make-sum (cons "- " x))
                     (make-factor x)))
               (make-atom f)))
          ((symbol? (car f))
           (let ((args (cdr f)))
             (case (car f)
               ((eq)
                (cond ((or (null? args)
                           (null? (cdr args)))
                       (error #f "<2 argument of eq" f))
                      (else
                       (make-statement
                        (cons (formula->tree (car args))
                              (map (lambda (x) (list " = " (tree->sum (formula->tree x))))
                                   (cdr args)))))))
               ((plus)
                (cond ((null? args)
                       (error #f "no argument of plus" f))
                      ((null? (cdr args))
                       (formula->tree (car args)))
                      (else
                       (make-sum
                        (cons (tree->sum (formula->tree (car args)))
                              (map (lambda (x) (list " + " (tree->product (formula->tree x))))
                                   (cdr args)))))))
               ((minus)
                (cond ((null? args)
                       (error #f "no argument of minus" f))
                      ((null? (cdr args))
                       (make-sum
                        (list "- " (tree->product (formula->tree (car args))))))
                      ((= (length args) 2)
                       (make-sum
                        (list (tree->sum (formula->tree (car args)))
                              " - "
                              (tree->product (formula->tree (cadr args))))))
                      (else
                       (error #f "more than two arguments of minus" f))))
               ((times)
                (cond ((null? args)
                       (error #f "no argument of times" f))
                      ((null? (cdr args))
                       (formula->tree (car args)))
                      (else
                       (make-product
                        (cons (tree->product (formula->tree (car args)))
                              (map (lambda (x) (list " " (tree->product (formula->tree x))))
                                   (cdr args)))))))
               ((divide)
                (cond ((or (null? args)
                           (null? (cdr args)))
                       (error #f "<2 argument of divide" f))
                      ((= (length args) 2)
                       (make-product
                        (list "\\frac{"
                              (formula->tree (car args))
                              "}{"
                              (formula->tree (cadr args))
                              "}")))
                      (else
                       (make-product
                        (list "\\frac{"
                              (formula->tree (car args))
                              "}{"
                              (tree->product (formula->tree (cadr args)))
                              (map (lambda (x) (list " " (tree->product (formula->tree x))))
                                   (cdddr f))
                              "}")))))
               ((abs)
                (cond ((= (length args) 1)
                       (make-factor
                        (list "\\lvert "
                              (formula->tree (car args))
                              " \\rvert")))
                      (else
                       (error #f "invalid number of arguments" f))))
               ((arccos arcsin arctan cos cosh cot coth csc exp ln sec sin sinh tan tanh)
                (cond ((= (length args) 1)
                       (make-factor
                        (list "\\"
                              (car f)
                              "("
                              (formula->tree (car args))
                              ")")))
                      (else
                       (error #f "invalid number of arguments" f))))
               ((arccosh arccot arccoth arccsc arccsch arcsec arcsech arcsinh arctanh csch sech)
                (cond ((= (length args) 1)
                       (make-factor
                        (list "\\operatorname{"
                              (car f)
                              "}("
                              (formula->tree (car args))
                              ")")))
                      (else
                       (error #f "invalid number of arguments" f))))
               ((ceiling)
                (cond ((= (length args) 1)
                       (make-factor
                        (list "\\lceil "
                              (formula->tree (car args))
                              " \\rceil")))
                      (else
                       (error #f "invalid number of arguments" f))))
               ((determinant)
                (cond ((= (length args) 1)
                       (make-factor
                        (list "\\det " (tree->product (formula->tree (car args))))))
                      (else
                       (error #f "invalid number of arguments" f))))
               ((floor)
                (cond ((= (length args) 1)
                       (make-factor
                        (list "\\lfloor "
                              (formula->tree (car args))
                              " \\rfloor")))
                      (else
                       (error #f "invalid number of arguments" f))))
               ((max min)
                (cond ((< (length args) 2)
                       (error #f "invalid number of arguments" f))
                      (else
                       (make-factor
                        (list "\\"
                              (car f)
                              "("
                              (formula->tree (car args))
                              (map (lambda (x) (list ", " (formula->tree x)))
                                   (cdr args))
                              ")")))))
               ((power)
                (cond ((< (length args) 2)
                       (error #f "<2 argument of power" f))
                      ((= (length args) 2)
                       (make-factor
                        (list (tree->product (formula->tree (car args)))
                              "^"
                              (tree->atom (formula->tree (cadr args))))))
                      (else
                       (error #f ">2 arguments of power" f))))
               ((root)
                (cond ((= (length args) 1)
                       (make-factor
                        (list "\\sqrt{"
                              (formula->tree (car args))
                              "}"))) ; TODO: Support degree other than 2
                      (else
                       (error #f "invalid number of arguments of root" f))))
               ((geq)
                (cond ((= (length args) 2)
                       (make-factor
                        (list (formula->tree (car args))
                              " \\geq "
                              (formula->tree (cadr args)))))
                      (else
                       (error #f "invalid number of arguments of geq" f))))
               ((gt)
                (cond ((= (length args) 2)
                       (make-factor
                        (list (formula->tree (car args))
                              " > "
                              (formula->tree (cadr args)))))
                      (else
                       (error #f "invalid number of arguments of gt" f))))
               ((leq)
                (cond ((= (length args) 2)
                       (make-factor
                        (list (formula->tree (car args))
                              " \\leq "
                              (formula->tree (cadr args)))))
                      (else
                       (error #f "invalid number of arguments of leq" f))))
               ((lt)
                (cond ((= (length args) 2)
                       (make-factor
                        (list (formula->tree (car args))
                              " < "
                              (formula->tree (cadr args)))))
                      (else
                       (error #f "invalid number of arguments of lt" f))))
               ((piece)
                (cond ((or (null? args)
                           (null? (cdr args)))
                       (error #f "<2 argument of piece" f))
                      (else
                       (list (formula->tree (car args))
                             " & \\text{if } "
                             (formula->tree (cadr args))))))
               ((otherwise)
                (cond ((null? args)
                       (error #f "no argument of otherwise" f))
                      (else
                       (list (formula->tree (car args))
                             " & \\text{otherwise}"))))
               ((piecewise)
                (cond ((null? args)
                       (error #f "no argument of piecewise" f))
                      (else
                       (make-factor
                        (list "\\begin{cases}"
                              (map (lambda (x) (list (formula->tree x) "\\\\")) args)
                              "\\end{cases}")))))
               ((diff)
                (cond ((or (null? args)
                           (null? (cdr args)))
                       (error #f "<2 argument of diff" f))
                      ((or (null? (car args))
                           (not (list? (car args)))
                           (not (symbol? (car (car args)))))
                       (error #f "unsupported argument of diff" f))
                      ((symbol=? 'bvar (car (car args)))
                       (let ((bvar-args (cdr (car args))))
                         (cond ((null? bvar-args)
                                (error #f "no argument of bvar" f))
                               ((= 1 (length bvar-args))
                                (make-factor
                                 (list "\\frac{\\operatorname{d}\\!"
                                       (tree->product (formula->tree (cadr args)))
                                       "}{\\operatorname{d}\\!"
                                       (formula->tree (car bvar-args))
                                       "}")))
                               ((= 2 (length bvar-args))
                                (let ((d (car bvar-args)))
                                  (cond ((or (not (list? d))
                                             (not (= (length d) 2))
                                             (not (symbol? (car d))))
                                         (error #f "unsupported argument of bvar" (car args)))
                                        ((and (symbol=? 'degree (car d))
                                              (number? (cadr d)))
                                         (make-factor
                                          (list "\\frac{\\operatorname{d}^{"
                                                (cadr d)
                                                "}\\!"
                                                (tree->product (formula->tree (cadr args)))
                                                "}{\\operatorname{d}\\!"
                                                (formula->tree (cadr bvar-args))
                                                "^{"
                                                (cadr d)
                                                "}}")))
                                        (else
                                         (error #f "unsupported argument of bvar" (car args))))))
                               (else
                                (error #f "unsupported argument of bvar" (car args))))))
                      (else
                       (error #f "unsupported argument of diff" f))))
               ((differential)
                (cond ((null? args)
                       (error #f "no argument of differential" f))
                      (else
                       (make-factor
                        (list "\\operatorname{d}\\!"
                              (tree->product (formula->tree (car args))))))))
               (else
                (error #f "unsupported formula" f)))))
          (else
           (error #f "unsupported formula" f))))

  (define (tree->string-list t)
    (cond ((number? t)
           (list (number->string t)))
          ((symbol? t)
           (list (symbol->string t)))
          ((string? t)
           (list t))
          ((list? t)
           (apply append (map tree->string-list t)))
          (else
           (assert (tree? t))
           (tree->string-list (tree-body t)))))

  (define (formula->latex f)
    (apply string-append (tree->string-list (formula->tree f))))

)
