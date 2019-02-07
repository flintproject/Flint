#!r6rs
(library (model lang)
  (export :default
          :independent
          :natural
          :real
          <
          variable-with-attribute
          variable
          parameter
          variable-form
          eq
          equation-form
          define-model
          )
  (import (except (rnrs (6)) <)
          (model macro)
          (model record))

  (define-auxiliary-keywords :default :independent :natural :real <)

  (define-syntax variable-with-attribute
    (syntax-rules (:default :independent :natural :real <)
      ((_ s)
       s)
      ((_ s :default value attr ...)
       (variable-with-attribute
        (make-variable
         (variable-name s)
         (variable-type s)
         (variable-set s)
         value)
        attr ...))
      ((_ s :independent attr ...)
       (variable-with-attribute
        (make-variable
         (variable-name s)
         'independent
         (variable-set s)
         (variable-default s))
        attr ...))
      ((_ s :natural attr ...)
       (variable-with-attribute
        (make-variable
         (variable-name s)
         (variable-type s)
         'natural
         (variable-default s))
        attr ...))
      ((_ s :real attr ...)
       (variable-with-attribute
        (make-variable
         (variable-name s)
         (variable-type s)
         'real
         (variable-default s))
        attr ...))
      ((_ s (< x y ...) attr ...) ; TODO
       (variable-with-attribute s attr ...))))

  (define-auxiliary-keywords variable parameter)

  (define-syntax variable-form
    (syntax-rules (variable parameter)
      ((_)
       '())
      ((_ (variable name attr ...) entry ...)
       (cons (variable-with-attribute (make-variable 'name 'dependent 'real #f) attr ...)
             (variable-form entry ...)))
      ((_ (parameter name attr ...) entry ...)
       (cons (variable-with-attribute (make-variable 'name 'parameter 'real #f) attr ...)
             (variable-form entry ...)))
      ((_ x entry ...)
       (variable-form entry ...))))

  (define-auxiliary-keywords eq)

  (define-syntax equation-form
    (syntax-rules (eq)
      ((_)
       '())
      ((_ (eq lhs rhs) entry ...)
       (cons (make-equation 'lhs 'rhs) (equation-form entry ...)))
      ((_ x entry ...)
       (equation-form entry ...))))

  (define-syntax define-model
    (syntax-rules ()
      ((_ name entry ...)
       (define name (make-model (variable-form entry ...) (equation-form entry ...))))))

)
