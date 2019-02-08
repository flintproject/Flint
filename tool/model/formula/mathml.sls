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

  (define (formula->mathml-list f prefix)
    (cond ((number? f)
           `(,@(open-tag "cn" prefix)
             ,(number->string f)
             ,@(close-tag "cn" prefix)))
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
