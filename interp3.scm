(load "prereqs.scm")

(display "Mutual recursion: it takes two to tango.")
(newline)

(define init-env
  (lambda (y)
    (if ((ceq? '=) y) c=
    (if ((ceq? '+) y) c+
    (if ((ceq? '*) y) c*
    (if ((ceq? 'add1) y) add1
    (if ((ceq? 'sub1) y) sub1
        error)))))))

(define value-of
  (lambda (exp)
    (lambda (env)
      (pmatch exp
        (,n (guard (number? n)) n)
        (,x (guard (symbol? x)) (env x))
        ((lambda (,x) ,body)
         (lambda (a)
           ((value-of body)
            (lambda (y) (if ((ceq? x) y) a (env y))))))
        ((if ,t ,c ,a)
         (if ((value-of t) env)
             ((value-of c) env)
             ((value-of a) env)))
        ((letrec ((,f ,rator1) (,g ,rator2)) ,body)
         ((value-of body)
          (letrec
            ((env^
              (lambda (y)
                (if ((ceq? f) y) ((value-of rator1) env^)
                (if ((ceq? g) y) ((value-of rator2) env^)
                    (env y))))))
            env^)))
        ((,rator ,rand)
         (((value-of rator) env) ((value-of rand) env)))))))

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

(test-check "odd? 5"
  (eval-exp '((letrec ((o (lambda (n)
                            (if ((= 0) n)
                                n
                                (e (sub1 n)))))
                       (e (lambda (n)
                            (if ((= 0) n)
                                1
                                (o (sub1 n))))))
                o)
              5))
  1)

(test-check "odd? 6"
  (eval-exp '((letrec ((o (lambda (n)
                            (if ((= 0) n)
                                n
                                (e (sub1 n)))))
                       (e (lambda (n)
                            (if ((= 0) n)
                                1
                                (o (sub1 n))))))
                o)
              6))
  0)




