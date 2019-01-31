(library (model cas)
  (export formula-simplify)
  (import (chezscheme)
          (model record))

  (define (leaves e)
    (cond ((null? e) '())
          ((number? e) '())
          ((symbol? e) (list e))
          (else
           (apply append (map leaves (cdr e))))))

  (define (valid-model? m)
    (let ((names (map variable-name (model-variables m))))
      (for-all
       (lambda (e)
         (for-all (lambda (s) (memq s names))
                  (append (leaves (equation-lhs e))
                          (leaves (equation-rhs e)))))
       (model-equations m))))

  (define (derivative? x)
    (and (list? x)
         (= 3 (length x))
         (eq? 'diff (car x))
         (eq? 'bvar (caadr x))))

  (define (derivative-iname d)
    (assert (derivative? d))
    (let* ((b (cadr d))
           (x (if (= (length b) 2)
                  (cadr b)
                  (caddr b))))
      (assert (symbol? x))
      x))

  (define (derivative-dname d)
    (assert (derivative? d))
    (let ((x (caddr d)))
      (assert (symbol? x))
      x))

  (define (derivative-degree d)
    (assert (derivative? d))
    (let ((x (cadr (cadr d))))
      (cond ((symbol? x) 1)
            ((and (list? x)
                  (= (length x) 2)
                  (eq? 'degree (car x)))
             (cadr x))
            (else
             (assertion-violation #f "invalid degree" x)))))

  (define (derivative-base d)
    (assert (derivative? d))
    (let ((iv (derivative-iname d))
          (dv (derivative-dname d)))
      `(diff (bvar ,iv) ,dv)))

  (define (unique l)
    (assert (list? l))
    (cond ((null? l) l)
          ((member (car l) (cdr l))
           (unique (cdr l)))
          (else
           (cons (car l) (unique (cdr l))))))

  (define (subformulae x)
    (cond ((symbol? x) (list x))
          ((number? x) (list x))
          ((null? x)
           (assertion-violation #f "formula required" x))
          ((derivative? x)
           (cons x (subformulae (caddr x))))
          (else
           (cons x (unique (apply append (map subformulae (cdr x))))))))

  (define (replace-derivative d s x)
    (assert (derivative? d))
    (assert (= (derivative-degree d) 1))
    (let ((vi (derivative-iname d))
          (vd (derivative-dname d)))
      (cond ((symbol? x) x)
            ((number? x) x)
            ((null? x) x)
            ((equal? x d) s)
            ((derivative? x)
             (if (and (eq? vi (derivative-iname x))
                      (eq? vd (derivative-dname x)))
                 (let ((degree (- (derivative-degree x) 1)))
                   (cond ((= degree 0) s)
                         ((= degree 1) `(diff (bvar ,vi) ,s))
                         (else `(diff (bvar (degree ,degree) ,vi) ,s))))
                 x))
            (else
             (cons (car x)
                   (map (lambda (y) (replace-derivative d s y)) (cdr x)))))))

  (define (derivative-free? x)
    (not (exists derivative? (subformulae x))))

  (define (equation-normal? e)
    (let ((lhs (equation-lhs e))
          (rhs (equation-rhs e)))
      (and (or (symbol? lhs)
               (and (derivative? lhs)
                    (= (derivative-degree lhs) 1)))
           (derivative-free? rhs))))

  (define (find-derivative x)
    (find derivative? (subformulae x)))

  (define (generate-variable n)
    (make-variable (string->symbol (string-append "X_" (number->string n)))
                   'dependent
                   'real
                   #f))

  (define (terminal-set x)
    (assert (derivative-free? x))
    (filter symbol? (subformulae x)))

  (define (set-minus s1 s2)
    (filter (lambda (e) (not (memq e s2))) s1))

  (define (terminal-set-minus x y)
    (assert (derivative-free? x))
    (assert (derivative-free? y))
    (set-minus (terminal-set x) (terminal-set y)))

  (define (solve s lhs rhs)
    (assert (symbol? s))
    (assert (list? lhs))
    (assert (not (null? lhs)))
    (case (car lhs)
      ((plus)
       (let lp ((args (cdr lhs))
                (term #f)
                (rest '()))
         (if (null? args)
             (if term
                 (make-equation term `(minus ,rhs ,@rest))
                 #f)
             (if (memq s (subformulae (car args)))
                 (lp (cdr args) (car args) rest)
                 (lp (cdr args) term (cons (car args) rest))))))
      ((minus)
       (cond ((= (length (cdr lhs)) 1)
              (assert (memq s (subformulae (cadr lhs))))
              (make-equation (cadr lhs) `(minus ,rhs)))
             ((memq s (subformulae (cadr lhs)))
              (make-equation (cadr lhs) `(plus ,rhs ,@(cddr lhs))))
             (else
              (make-equation `(plus ,(cadr lhs) ,@(map (lambda (x) `(minus ,x)) (cddr lhs))) rhs))))
      ((times)
       (let lp ((args (cdr lhs))
                (term #f)
                (rest '()))
         (if (null? args)
             (if term
                 (make-equation term `(divide ,rhs ,@rest))
                 #f)
             (if (memq s (subformulae (car args)))
                 (lp (cdr args) (car args) rest)
                 (lp (cdr args) term (cons (car args) rest))))))
      ((divide)
       (assert (<= 3 (length lhs)))
       (cond ((memq s (subformulae (cadr lhs)))
              (make-equation (cadr lhs) `(times ,rhs ,@(cddr lhs))))
             (else
              (make-equation `(times ,(cadr lhs) ,@(map (lambda (x) `(divide 1 ,x)) (cddr lhs))) rhs))))
      (else
       #f)))

  (define (normalize m)
    (let-values (((undefined defined) (partition (lambda (v) (symbol=? (variable-type v) 'dependent)) (model-variables m))))
      (let lp ((n 0)
               (undefined undefined)
               (defined defined)
               (rest (model-equations m))
               (done '()))
        (cond ((null? rest)
               (if (null? undefined)
                   (make-model defined (reverse done))
                   (error #f "undefined variables" undefined)))
              ((<= 1000 n) ; indicating that the normalizing algorithm is failing
               (error #f "normalization failed" rest))
              (else
               (let* ((e (car rest))
                      (lhs (equation-lhs e))
                      (rhs (equation-rhs e)))
                 (cond ((number? lhs)
                        (lp (+ n 1)
                            undefined
                            defined
                            (cons (make-equation rhs lhs) (cdr rest))
                            done))
                       ((symbol? lhs)
                        (if (find-derivative rhs)
                            (lp (+ n 1)
                                undefined
                                defined
                                (cons (make-equation rhs lhs) (cdr rest))
                                done)
                            (let-values (((defined0 undefined1) (partition (lambda (v) (symbol=? (variable-name v) lhs)) undefined)))
                              (lp (+ n 1)
                                  undefined1
                                  (append defined defined0)
                                  (cdr rest)
                                  (cons e done)))))
                       ((derivative? lhs)
                        (if (= (derivative-degree lhs) 1)
                            (let ((d (find-derivative rhs)))
                              (if d
                                  (error #f "equation of unsupported form" e) ; TODO
                                  (let-values (((defined0 undefined1) (partition (lambda (v) (symbol=? (variable-name v) (derivative-dname lhs))) undefined)))
                                    (lp n
                                        undefined1
                                        (append defined defined0)
                                        (cdr rest)
                                        (cons e done)))))
                            (let* ((b (derivative-base lhs))
                                   (v (generate-variable n))
                                   (s (variable-name v)))
                              (lp (+ n 1)
                                  (cons v undefined)
                                  defined
                                  `(,(make-equation b s)
                                    ,(make-equation (replace-derivative b s lhs)
                                                    (replace-derivative b s rhs))
                                    ,@(cdr rest))
                                  done))))
                       (else
                        (let ((ld (find-derivative lhs)))
                          (if ld
                              (let* ((b (derivative-base ld))
                                     (v (generate-variable n))
                                     (s (variable-name v)))
                                (lp (+ n 1)
                                    (cons v undefined)
                                    defined
                                    `(,(make-equation b s)
                                      ,(make-equation (replace-derivative b s lhs)
                                                      (replace-derivative b s rhs))
                                      ,@(cdr rest))
                                    done))
                              (cond ((find-derivative rhs)
                                     (lp (+ n 1)
                                         undefined
                                         defined
                                         (cons (make-equation rhs lhs) (cdr rest))
                                         done))
                                    (else
                                     (let ((ts (set-minus (terminal-set-minus lhs rhs) (map variable-name defined))))
                                       (cond ((null? ts)
                                              (lp (+ n 1)
                                                  undefined
                                                  defined
                                                  (cons (make-equation rhs lhs) (cdr rest))
                                                  done))
                                             (else
                                              (let ((r (solve (car ts) lhs rhs)))
                                                (if r
                                                    (lp (+ n 1)
                                                        undefined
                                                        defined
                                                        (cons r (cdr rest))
                                                        done)
                                                    (lp (+ n 1)
                                                        undefined
                                                        defined
                                                        `(,@(cdr rest) ,e)
                                                        done))))))))))))))))))

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
              ((null? (cdr rgs))
               (cond ((= 0 a) ; (minus 0 b) -> (minus b)
                      `(minus ,(car rgs)))
                     ((= 0 (car rgs)) ; (minus a 0) -> a
                      a)
                     ((equal? a (car rgs)) ; (minus a a) -> 0
                      0)
                     (else #f)))
              ((memq 0 rgs) ; (minus a b ... 0 c ...) -> (minus a b ... c ...)
               `(minus ,a ,@(remq 0 rgs)))
              ((= 0 a) ; (minus 0 b ...) -> (minus (plus b ...))
               `(minus (plus ,@rgs)))
              ((member a rgs) ; (minus a b ... a c ...) -> (minus (plus b ... c ...))
               => (lambda (rest)
                    `(minus (plus ,@(reverse (list-tail (reverse rgs) (length rest))) ,@(cdr rest)))))
              (else #f))))

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
      (assert (<= 2 (length args)))
      (cond ((= 0 (car args)) ; (divide 0 a ...) -> 0
             0)
            ((remq 1 (cdr args)) ; (divide a b ... 1 c ...) -> (divide a b ... c ...)
             => (lambda (rest)
                  (if (null? rest) ; (divide a 1 ...) -> a
                      (car args)
                      `(divide ,(car args) ,@rest))))
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
