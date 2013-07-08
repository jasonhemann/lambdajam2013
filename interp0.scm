(load "prereqs.scm")

(display "combinator evaluator, seeded with primitive combinators (curried) and atoms")
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
;;      add numbers
        [,x (guard (symbol? x)) (env x)]
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




