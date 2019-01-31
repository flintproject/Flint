(library (model macro)
  (export define-auxiliary-keywords)
  (import (chezscheme))

  ;; Adopted from <https://github.com/cisco/ChezScheme/issues/41>

  (define-syntax define-auxiliary-keyword
    (syntax-rules ()
      ((_ name)
       (define-syntax name
         (lambda (x)
           (syntax-violation #f "unexpected auxiliary keyword" x))))))

  (define-syntax define-auxiliary-keywords
    (syntax-rules ()
      ((_ name* ...)
       (begin
         (define-auxiliary-keyword name*)
         ...))))

)
