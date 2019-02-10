#!r6rs
(library (model formula mathml)
  (export formula->mathml)
  (import (rnrs (6)))

  (define (open-tag name prefix)
    (if prefix
        `("<" ,prefix ":" ,name ">")
        `("<" ,name ">")))

  (define (close-tag name prefix)
    (if prefix
        `("</" ,prefix ":" ,name ">")
        `("</" ,name ">")))

  (define (empty-tag name prefix)
    (if prefix
        `("<" ,prefix ":" ,name "/>")
        `("<" ,name "/>")))

  (define (open-cn type prefix)
    (let ((attr (if type `(" type=\"" ,type "\"") '())))
      (if prefix
          `("<" ,prefix ":cn" ,@attr ">")
          `("<cn" ,@attr ">"))))

  (define (number->cn x prefix)
    (if (and (exact? x)
             (not (integer? x))
             (rational? x))
        `(,@(open-cn "rational" prefix)
          ,(number->string (numerator x))
          ,@(empty-tag "sep" prefix)
          ,(number->string (denominator x))
          ,@(close-tag "cn" prefix))
        `(,@(open-cn #f prefix)
          ,(number->string x)
          ,@(close-tag "cn" prefix))))

  (define (formula->mathml-list f prefix)
    (cond ((number? f)
           (number->cn f prefix))
          ((symbol? f)
           `(,@(open-tag "ci" prefix)
             ,(symbol->string f)
             ,@(close-tag "ci" prefix)))
          (else
           (let ((name (symbol->string (car f)))
                 (args (cdr f)))
             (case (car f)
               ((bvar degree)
                (append
                 (open-tag name prefix)
                 (apply append (map (lambda (x) (formula->mathml-list x prefix)) args))
                 (close-tag name prefix)))
               (else
                (append
                 (open-tag "apply" prefix)
                 (empty-tag name prefix)
                 (apply append (map (lambda (x) (formula->mathml-list x prefix)) args))
                 (close-tag "apply" prefix))))))))

  (define (formula->mathml f prefix)
    (apply string-append (formula->mathml-list f prefix)))

)
