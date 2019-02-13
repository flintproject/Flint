#!r6rs
(library (model formula simpl)
  (export formula-simplify)
  (import (rnrs (6)))

  (define (formula-simplify x)

    (define (reduce-plus args)
      (cond ((null? args) ; (plus) -> 0
             0)
            ((null? (cdr args)) ; (plus a) -> a
             (car args))
            ((memq 0 args) ; (plus a ... 0 b ...) -> (plus a ... b ...)
             `(plus ,@(remq 0 args)))
            (else #f)))

    (define (reduce-minus args)
      (let ((a (car args))
            (rgs (cdr args)))
        (cond ((null? rgs)
               (cond ((number? a) ; (minus num) -> -num
                      (- a))
                     ((and (list? a)
                           (= (length a) 2)
                           (eq? 'minus (car a))) ; (minus (minus b)) -> b
                      (cadr a))
                     (else #f)))
              (else
               (let ((b (car rgs)))
                 (cond ((and (number? a) (= 0 a)) ; (minus 0 b) -> (minus b)
                        `(minus ,b))
                       ((and (number? b) (= 0 b)) ; (minus a 0) -> a
                        a)
                       ((equal? a b) ; (minus a a) -> 0
                        0)
                       ((and (list? b)
                             (= (length b) 2)
                             (eq? 'minus (car b))) ; (minus a (minus c)) -> (plus a c)
                        `(plus ,a ,(cadr b)))
                       (else #f)))))))

    (define (reduce-times args)
      (cond ((null? args) ; (times) -> 1
             1)
            ((null? (cdr args)) ; (times a) -> a
             (car args))
            ((memq 0 args) ; (times a ... 0 b ...) -> 0
             0)
            ((memq 1 args) ; (times a ... 1 b ...) -> (times a ... b ...)
             `(times ,@(remq 1 args)))
            (else #f)))

    (define (reduce-divide args)
      (assert (= 2 (length args)))
      (cond ((and (number? (car args)) (= 0 (car args))) ; (divide 0 a) -> 0
             0)
            ((and (number? (cadr args)) (= 1 (cadr args))) ; (divide a 1) -> a
             (car args))
            (else #f)))

    (define (reduce-formula x)
      (cond ((number? x) #f)
            ((symbol? x) #f)
            ((reduce-subformulae (cdr x))
             => (lambda (vargs) (cons (car x) vargs)))
            (else
             (let ((args (cdr x)))
               (case (car x)
                 ((plus)
                  (reduce-plus args))
                 ((minus)
                  (reduce-minus args))
                 ((times)
                  (reduce-times args))
                 ((divide)
                  (reduce-divide args))
                 (else #f))))))

    (define (reduce-subformulae args)
      (let ((vargs (map reduce-formula args)))
        (and (exists (lambda (x) x) vargs)
             (map (lambda (a b) (or a b)) vargs args))))

    (let lp ((x x)
             (y (reduce-formula x)))
      (if y
          (lp y (reduce-formula y))
          x)))

)
