(load "prereqs.scm")

(display "A reducer for cbv-reduction to weak normal form")
(newline)

;; lambda is disallowed as formal parameter. In order to support that,
;; subst can be defined as follows:

(define alpha-all
  (lambda (exp)
    (pmatch exp
      [,x (guard (symbol? x)) x]
      [(lambda (,x) ,body)
       (let ((g (gensym (symbol->string x))))
         `(lambda (,g) ,(subst g x (alpha-all body))))]
      [(,rator ,rand)
       `(,(alpha-all rator) ,(alpha-all rand))])))

(define self
  (lambda (under before)
    (lambda (str)      
      (letrec
        ((self (lambda (exp)
                 (pmatch exp
                   [,x (guard (symbol? x)) x]
                   [(lambda (,x) ,body)
                    `(lambda (,x) ,((under self) body))]
                   [(,rator ,rand)
                    (pmatch (self rator)
                      [(lambda (,x) ,body)
                       (str (subst ((before self) rand) x (alpha-all body)))]
                      [,else `(,else ,((before self) rand))])]))))      
        self))))

(define yes (lambda (f) (lambda (x) (f x))))
(define no (lambda (f) (lambda (x) x)))

(define by-value (self no yes))

(define bv-wnf
  (letrec ((str (lambda (exp) ((by-value str) exp))))
    str))

(print-gensym 'pretty/suffix)

(test-check "test 1"
  (let ((fn (bv-wnf '((lambda (x) (lambda (x) x)) z))))
    ((eval fn) 5))  
  5)

(test-check "test 5"
  (bv-wnf '((lambda (x) y) z))
  'y)

(test-check "test 8"
  (bv-wnf '((lambda (x) x) z))
  'z)

(test-check "complex-countdown"
  (let ((fn (bv-wnf
              '(((lambda (w) (w w))
                 (lambda (f)
                   (lambda (n)
                     ((lambda (id)
                        ((n (lambda (_)
                              ((f f) (lambda (f)
                                       (lambda (x)
                                         (((n (lambda (g)
                                                (lambda (h)
                                                  (h (g f)))))
                                           (lambda (u) x))
                                          id))))))
                         id))
                      (lambda (f) f)))))
                (lambda (f) (lambda (x) (f (f (f (f (f x)))))))))))
    (((eval fn) add1) 0))
  1)
