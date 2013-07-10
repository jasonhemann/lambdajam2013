(load "prereqs.scm")

(display "splitting the environment")
(newline)

(define init-senv
  (lambda (y)
    (if ((ceq? '+) y) 0
    (if ((ceq? '*) y) 1
    (if ((ceq? 'add1) y) 2
    (if ((ceq? 'sub1) y) 3
        error))))))

(define init-denv
  (lambda (y)
    (if ((c= 0) y) c+
    (if ((c= 1) y) c*
    (if ((c= 2) y) add1
    (if ((c= 3) y) sub1
        error))))))

(define value-of
  (lambda (exp)
    (lambda (senv)
      (pmatch exp
        (,n (guard (number? n)) (lambda (denv) n))
        (,x (guard (symbol? x)) (lambda (denv) (denv (senv x))))
        ((lambda (,x) ,body)
         (lambda (denv)
           (lambda (a)
             (((value-of body)
               (lambda (y)
                 (if ((ceq? x) y) 0 (add1 (senv y)))))
              (lambda (y)
                (if ((c= 0) y) a (denv (sub1 y))))))))
        ((,rator ,rand)
         (lambda (denv)
           ((((value-of rator) senv) denv) (((value-of rand) senv) denv))))))))

(define eval-exp
  (lambda (exp)
    (((value-of exp) init-senv) init-denv)))

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
  (eval-exp
   '(((lambda (y)
        (lambda (x)
          ((+ x) (sub1 y))))
      15)
     11))
  25)

(test-check "complex countdown"
  (eval-exp complex-countdown)
  1)

