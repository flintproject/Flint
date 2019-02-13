#!r6rs
(library (model formula simpl)
  (export formula-simplify)
  (import (rnrs (6)))

  (define (exact-number? x)
    (and (number? x) (exact? x)))

  (define (formula-simplify x)

    (define (reduce-plus args)
      (cond ((null? args) ; (plus) -> 0
             0)
            ((null? (cdr args)) ; (plus a) -> a
             (car args))
            ((memq 0 args) ; (plus a ... 0 b ...) -> (plus a ... b ...)
             `(plus ,@(remq 0 args)))
            ((and (< 0 (length args))
                  (list? (car args))
                  (not (null? (car args)))
                  (eq? 'plus (caar args))) ; (plus (plus a ...) b ...) -> (plus a ... b ...)
             `(plus ,@(cdar args) ,@(cdr args)))
            (else
             (let-values (((x y) (partition exact-number? args)))
               ;; place sum of exact numbers at the end
               (cond ((null? x)
                      #f)
                     ((= 1 (length x))
                      (if (exact-number? (car (reverse args)))
                          #f
                          `(plus ,@y ,(car x))))
                     (else
                      `(plus ,@y ,(apply + x))))))))

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
              ((for-all exact-number? args) ; (minus m n) -> m-n
               (apply - args))
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
                       ((and (list? b)
                             (= (length b) 3)
                             (eq? 'minus (car b))) ; (minus a (minus c1 c2)) -> (minus (plus a c2) c1)
                        `(minus (plus ,a ,(caddr b)) ,(cadr b)))
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
            (else
             (let-values (((x y) (partition exact-number? args)))
               ;; place product of exact numbers at the beginning
               (cond ((null? x)
                      #f)
                     ((= 1 (length x))
                      (if (exact-number? (car args))
                          #f
                          `(times ,(car x) ,@y)))
                     (else
                      `(times ,(apply * x) ,@y)))))))

    (define (reduce-divide args)
      (assert (= 2 (length args)))
      (cond ((and (number? (car args)) (= 0 (car args))) ; (divide 0 a) -> 0
             0)
            ((and (number? (cadr args)) (= 1 (cadr args))) ; (divide a 1) -> a
             (car args))
            ((for-all exact-number? args) ; (divide n d) -> n/d
             (apply / args))
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
