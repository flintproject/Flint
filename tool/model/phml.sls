#!r6rs
(library (model phml)
  (export model->phml
          display-phml)
  (import (model cas)
          (model record)
          (model formula mathml)
          (chezscheme))

  (define-record-type definition
    (fields type
            mathml))

  (define-record-type physical-quantity
    (fields name
            type
            implementation
            initial-value))

  (define-record-type phml
    (fields uuid
            physical-quantities))

  (define (replace-independent-variable-with-time f m)
    (let ((x (variable-name (model-independent-variable m))))
      (cond ((number? f)
             f)
            ((symbol? f)
             (if (symbol=? x f)
                 'time
                 f))
            (else
             (cons (car f)
                   (map (lambda (g) (replace-independent-variable-with-time g m)) (cdr f)))))))

  (define (find-ae v m)
    (cond ((find
            (lambda (e)
              (eq? (variable-name v) (equation-lhs e)))
            (model-equations m))
           => (lambda (e)
                `(eq ,(equation-lhs e) ,(equation-rhs e))))
          (else #f)))

  (define (find-ode v m)
    (cond ((find
            (lambda (e)
              (equal? `(diff (bvar ,(variable-name (model-independent-variable m))) ,(variable-name v))
                      (equation-lhs e)))
            (model-equations m))
           => (lambda (e)
                `(eq ,(equation-lhs e) ,(equation-rhs e))))
          (else #f)))

  (define (model->phml m uuid)
    (let ((n (normalize m)))
      (make-phml uuid
                 (map
                  (lambda (v)
                    (cond ((symbol=? (variable-type v) 'parameter)
                           (make-physical-quantity
                            (variable-name v)
                            'static-parameter
                            (make-definition 'ae `(eq ,(variable-name v) ,(or (variable-default v) 0)))
                            #f))
                          ((find-ae v n)
                           => (lambda (ae)
                                (make-physical-quantity
                                 (variable-name v)
                                 'variable-parameter
                                 (make-definition 'ae (replace-independent-variable-with-time ae m))
                                 #f)))
                          ((find-ode v n)
                           => (lambda (ode)
                                (make-physical-quantity
                                 (variable-name v)
                                 'state
                                 (make-definition 'ode (replace-independent-variable-with-time ode m))
                                 (make-definition 'ae `(eq ,(variable-name v) ,(or (variable-default v) 0))))))
                          (else
                           (error #f "failed to convert to PHML" v))))
                  (remp (lambda (v) (symbol=? (variable-type v) 'independent))
                        (model-variables n))))))

  (define (implementation->list impl)
    `("<is:implementation>\n"
      "<is:definition type=\"" ,(definition-type impl) "\" format=\"mathml\">\n"
      "<m:math>"
      ,(formula->mathml (definition-mathml impl) "m")
      "</m:math>\n"
      "</is:definition>\n"
      "</is:implementation>\n"
      ))

  (define (initial-value->list iv)
    (if iv
        `("<is:initial-value>\n"
          "<is:definition type=\"" ,(definition-type iv)  "\" format=\"mathml\">\n"
          "<m:math>"
          ,(formula->mathml (definition-mathml iv) "m")
          "</m:math>\n"
          "</is:definition>\n"
          "</is:initial-value>\n"
          )
        '()))

  (define (physical-quantity->list pq id)
    `("<is:physical-quantity type=\"" ,(physical-quantity-type pq) "\" physical-quantity-id=\"" ,(+ id 1) "\">\n"
      "<is:name>" ,(physical-quantity-name pq) "</is:name>\n"
      ,@(implementation->list (physical-quantity-implementation pq))
      ,@(initial-value->list (physical-quantity-initial-value pq))
      "<is:dimension type=\"scalar\" />\n"
      "<is:value-type-set>\n"
      "<is:value-type unit-id=\"0\" precision=\"double\" />\n"
      "</is:value-type-set>\n"
      "</is:physical-quantity>\n"
     ))

  (define (phml->list p)
    (assert (phml? p))
    `("<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
      "<is:insilico-model xmlns:m=\"http://www.w3.org/1998/Math/MathML\" xmlns:is=\"http://www.physiome.jp/ns/insilicoml\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:schemalocation=\"http://www.physiome.jp/ns/insilicoml-1.0 insilicoml.xsd\" version=\"1.0\">\n"
      "<is:header>\n"
      "  <is:numerical-configuration>\n"
      "    <is:time-discretization unit-id=\"3\" evolution=\"true\">\n"
      "      <is:step>0.01</is:step>\n"
      "    </is:time-discretization>\n"
      "    <is:simulation-time-span unit-id=\"3\">100</is:simulation-time-span>\n"
      "    <is:algorithm>\n"
      "      <is:integration name=\"4th-rungekutta\"/>\n"
      "    </is:algorithm>\n"
      "  </is:numerical-configuration>\n"
      "</is:header>\n"
      "<is:unit-set>\n"
      "  <is:unit unit-id=\"0\">\n"
      "    <is:name>dimensionless</is:name>\n"
      "  </is:unit>\n"
      "  <is:unit unit-id=\"1\">\n"
      "    <is:name>meter</is:name>\n"
      "  </is:unit>\n"
      "  <is:unit unit-id=\"2\">\n"
      "    <is:name>kilogram</is:name>\n"
      "  </is:unit>\n"
      "  <is:unit unit-id=\"3\">\n"
      "    <is:name>second</is:name>\n"
      "  </is:unit>\n"
      "  <is:unit unit-id=\"4\">\n"
      "    <is:name>ampere</is:name>\n"
      "  </is:unit>\n"
      "  <is:unit unit-id=\"5\">\n"
      "    <is:name>kelvin</is:name>\n"
      "  </is:unit>\n"
      "  <is:unit unit-id=\"6\">\n"
      "    <is:name>mole</is:name>\n"
      "  </is:unit>\n"
      "  <is:unit unit-id=\"7\">\n"
      "    <is:name>candela</is:name>\n"
      "  </is:unit>\n"
      "</is:unit-set>\n"
      "<is:module-set>\n"
      "  <is:module module-id=\"" ,(phml-uuid p) "\" type=\"functional-unit\">\n"
      "    <is:property>\n"
      "      <is:name>M</is:name>\n"
      "      <is:capsulation state=\"false\"/>\n"
      "      <is:template state=\"false\"/>\n"
      "    </is:property>\n"
      "    <is:physical-quantity-set>\n"
      ,@(apply
         append
         (map
          physical-quantity->list
          (phml-physical-quantities p)
          (iota (length (phml-physical-quantities p)))))
      "    </is:physical-quantity-set>\n"
      "  </is:module>\n"
      "</is:module-set>\n"
      "</is:insilico-model>\n"
      ))

  (define (display-phml p)
    (for-each display (phml->list p)))

)
