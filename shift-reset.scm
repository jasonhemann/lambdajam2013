(load "prereqs.scm")

(display "A similar interpreter for a language with shift and reset")
(newline)

(define c+
  (lambda (v)
    (lambda (k)
      (k (lambda (w)
           (lambda (k)
             (k (+ v w))))))))

(define c*
  (lambda (v)
    (lambda (k)
      (k (lambda (w)
           (lambda (k)
             (k (* v w))))))))

(define cadd1
  (lambda (v)
    (lambda (k)
      (k (add1 v)))))

(define csub1
  (lambda (v)
    (lambda (k)
      (k (sub1 v)))))

(define init-env
  (lambda (y)
    (if ((ceq? '*) y) c*
    (if ((ceq? '+) y) c+
    (if ((ceq? 'add1) y) cadd1
    (if ((ceq? 'sub1) y) csub1
        error))))))

(define empty-g
  (lambda (v) v))

(define empty-k
  (lambda (x)
    (lambda (g)
      (g x))))

(define shift-fun
  (lambda (k)
    (lambda (a)
      (lambda (k^)
        (lambda (g^)
          ((k a)
           (lambda (w)
             ((k^ w) g^))))))))

(define value-of
  (lambda (exp)
    (lambda (env)      
      (pmatch exp
        (,exp (guard (number? exp)) (lambda (k) (k exp)))
        (,exp (guard (symbol? exp))
         (let ((sval (env exp)))
           (lambda (k) (k sval))))
        ((lambda (,x) ,body)
         (let ((sbody (value-of body)))           
           (let ((sfun (lambda (a)
                         (sbody
                          (lambda (y)
                            (if ((ceq? x) y) a (env y)))))))
             (lambda (k) (k sfun)))))          
        ((shift ,rator)
         (let ((srator ((value-of rator) env)))
           (lambda (k)
             (srator
              (lambda (p)
                ((p (shift-fun k)) empty-k))))))
        ((reset ,exp)
         (let ((sexp ((value-of exp) env)))
           (lambda (k)
             (lambda (g)  
               ((sexp empty-k)
                (lambda (v) ((k v) g)))))))
        ((,rator ,rand)
         (let ((srator ((value-of rator) env)))
           (let ((srand ((value-of rand) env)))
             (lambda (k)
               (srator (lambda (p)
                         (srand (lambda (a)
                                  ((p a) k)))))))))))))

(define eval-exp
  (lambda (exp)
    ((((value-of exp) init-env) empty-k) empty-g)))

;;;;;;;;;;;;;;;;; Test Suite

(test-check "lambda-calc"
  (eval-exp '((lambda (x) ((lambda (y) x) 6)) 5))
  5)

(test-check "worse lambda calc"
  (eval-exp '(((lambda (x) (lambda (y) x)) 5) 6))
  5)

(test-check "test1"
  (eval-exp '(add1 (reset (add1 (shift (lambda (k) (add1 (k (add1 (k 1))))))))))
  6)

(test-check "test2"
  (eval-exp '(add1 (reset (add1 (shift (lambda (k) (add1 (k 1))))))))
  4)

(test-check "test3"  
  (eval-exp '(add1 (reset (add1 (shift (lambda (k) (k (k (k (k (k (k 1))))))))))))
  8)

(test-check "test4"
  (eval-exp '(add1 (reset ((+ 5) (shift (lambda (k) (k (k (k (k (k (k 1))))))))))))
  32)

(test-check "s&r"
  (eval-exp '(reset ((+ (shift (lambda (k) ((+ 10) (k 100))))) (shift (lambda (k^) 1)))))
  11)

(test-check "working test1"
  (eval-exp '(reset ((+ (shift (lambda (k) ((+ 10) (k 100))))) (shift (lambda (k^) 1)))))
  11)

(test-check "working-test2"
  (eval-exp '(add1 (reset (add1 (shift (lambda (k) (k (add1 (k (k 5))))))))))
  10)

(test-check "working test3"
  (eval-exp '(reset (add1 (shift (lambda (k) (k 5))))))
  6)

(test-check "other s&r"
  (eval-exp '(reset (add1 ((+ (shift (lambda (k) ((+ 10) (k 100))))) (shift (lambda (k^) (add1 1)))))))
  12)

(test-check "nested resets"
  (eval-exp '(reset (add1 (shift (lambda (k) (add1 (reset (shift (lambda (k2) (k 5))))))))))
  7)

(test-check "nested resets"
  (eval-exp '(reset (add1 (add1 (shift (lambda (k) (add1 (reset (shift (lambda (k2) (k 5)))))))))))
  8)

(test-check "nested resets"
  (eval-exp '(reset (add1 (add1 (shift (lambda (k) (reset (add1 (shift (lambda (k2) (k 5)))))))))))
  7)

(test-check "nested resets"
  (eval-exp '(reset (add1 (add1 (shift (lambda (k) (reset (add1 (shift (lambda (k2) (k2 (k 5))))))))))))
  8)

(test-check "nested resets"
  (eval-exp '(reset (add1 (add1 (shift (lambda (k) (reset (add1 (shift (lambda (k2) (k (k2 5))))))))))))
  8)

(test-check "nested resets"
  (eval-exp '(reset (add1 (add1 (shift (lambda (k) (reset (k (add1 (shift (lambda (k2) (k2 5)) ))))))))))
  8)

(test-check "nested resets"
  (eval-exp '(reset (add1 (add1 (shift (lambda (k) (reset (k (add1 (shift (lambda (k2) (k2 (k2 5)))))))))))))
  11)

(test-check "plus and times"
  (eval-exp '((* 10) (reset ((* 20) (shift (lambda (k) ((+ 5) (k ((+ 2) 3)))))))))
  1050)