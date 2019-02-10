#!r6rs
(import (model lang)
        (model phml)
        (chezscheme))

(unless (= (length (command-line)) 3)
  (error #f "usage: model2phml PATH UUID" (command-line)))
(let ((p (cadr (command-line)))
      (uuid (caddr (command-line))))
  (load p)
  (let* ((name (path-last (path-root p)))
         (m (eval (string->symbol name))))
    (display-phml (model->phml m uuid))))
