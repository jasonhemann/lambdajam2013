(load "prereqs.scm")

(display "adding a feature -- eval-in")
(newline)

(define init-env
  (lambda (y)
    (if ((ceq? '+) y) c+
    (if ((ceq? '*) y) c*
    (if ((ceq? 'add1) y) add1
    (if ((ceq? 'sub1) y) sub1
        error))))))

(define value-of
  (lambda (exp)
    (lambda (env)
      (pmatch exp
        [,n (guard (number? n)) n]
        [,x (guard (symbol? x)) (env x)]
        [(call/cenv ,rator)
         (((value-of rator) env) env)]
        [(eval-in ,exp)
;;       complete the eval-in line
         ]
        [(lambda (,x) ,body)
         (lambda (a)
           ((value-of body)
            (lambda (y) (if ((ceq? x) y) a (env y)))))]
        [(,rator ,rand)
         (((value-of rator) env) ((value-of rand) env))]))))

(define eval-exp
  (lambda (exp)
    ((value-of exp) init-env)))

;;;;;;;;;;;;;;;;; Test Suite

(load "test-data.scm")

(test-check "fifteen is 15"
  (eval-exp '15)
  15)

(test-check "basic interp"
  (eval-exp '((* 5) ((+ 5) 6)))
  55)

(test-check "application"
  (eval-exp '((lambda (x) x) 5))
  5)

(test-check "basic lambda-calc test"
  (eval-exp '(((lambda (y)
                  (lambda (x)
                    ((+ x) (sub1 y))))
                15)
               11))
  25)

(test-check "complex countdown"
  (eval-exp complex-countdown)
  1)

(test-check "let-env"
  (eval-exp
   '((lambda (x)
       (call/cenv
         (lambda (env) x)))
     5))
  5)

(test-check "let-env and value-of"
  (eval-exp
   '((lambda (x)
       (call/cenv
         (lambda (env)
           ((eval-in x) env))))
     5))
  5)

(test-check "let-env and value-of, simply"
  (eval-exp
   '((lambda (x)
       (call/cenv (eval-in x)))
     5))
  5)

;; introducing let-env (and value-of) broke alpha equivalence

(test-check "with one version"
  (eval-exp
   '((lambda (x)
       (call/cenv
         (lambda (env)
           ((lambda (x) ((eval-in x) env))
            6))))
     5))
  5)

(test-check "but with the other"
  (eval-exp
   '((lambda (x)
       (call/cenv
         (lambda (env)
           ((lambda (y) ((eval-in y) env))
            6))))
     5))
  error)
