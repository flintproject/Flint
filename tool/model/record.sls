#!r6rs
(library (model record)
  (export make-variable
          variable-name
          variable-type
          variable-set
          variable-default
          make-equation
          equation-lhs
          equation-rhs
          make-model
          model-variables
          model-equations
          print-model
          )
  (import (rnrs (6)))

  (define-record-type variable
    (fields name
            type
            set
            default))

  (define-record-type equation
    (fields lhs
            rhs))

  (define-record-type model
    (fields variables
            equations))

  (define (print-model m)
    (define (print-list name l)
      (display ";; ")
      (display name)
      (newline)
      (for-each (lambda (v) (display v) (newline)) l))
    (print-list 'variables (model-variables m))
    (print-list 'equations (model-equations m)))

)
