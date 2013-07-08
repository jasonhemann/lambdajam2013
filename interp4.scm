(load "dprereqs.scm")

(display "extent-based variable bindings.")
(newline)

(define init-env
  (lambda (y)
    (if ((ceq? '=) y) dc=
    (if ((ceq? '+) y) dc+
    (if ((ceq? '*) y) dc*
    (if ((ceq? 'add1) y) dcadd1
    (if ((ceq? 'sub1) y) dcsub1
        error)))))))

(define value-of
  (lambda (exp)
    (lambda (env)
      (pmatch exp
        [,n (guard (number? n)) n]
        [,x (guard (symbol? x)) (env x)]
        [(lambda (,x) ,body)
;;       complete the lambda line
         ]
        [(if ,t ,c ,a)
         (if ((value-of t) env)
             ((value-of c) env)
             ((value-of a) env))]
        [(,rator ,rand)
         ((((value-of rator) env) env) ((value-of rand) env))]))))

(define eval-exp
  (lambda (exp)
    ((value-of exp) init-env)))

;;;;;;;;;;;;;;;;; Test Suite

(load "test-data.scm")

(test-check "fifteen is 15"
  (eval-exp '15)
  15)

(test-check "basic interp with env-receiving primitives"
  (eval-exp '((* 5) ((+ 5) 6)))
  55)

(test-check "application"
  (eval-exp '((lambda (x) x) 5))
  5)

(test-check "with arithmetic operators"
  (eval-exp
   '((lambda (a)
       ((lambda (p)
          ((lambda (a)
             ((* a) (p 2)))
           5))
        (lambda (x) ((+ x) a))))
     3))
  35)

(test-check "dynamic !5"
  (eval-exp
   '((lambda (!)
       (! 5))
     (lambda (n)
       (if ((= 0) n)
           1
           ((* n) (! (sub1 n)))))))
  120)

(test-check "without collision"
  (eval-exp
   '((lambda (x)
       ((lambda (y)
          ((lambda (a)
             (y y))
           5))
        (lambda (z) x)))
     6))
  6)

(test-check "with collision"
  (eval-exp
   '((lambda (x)
       ((lambda (y)
          ((lambda (x)
             (y y))
           5))
        (lambda (z) x)))
     6))
  5)


