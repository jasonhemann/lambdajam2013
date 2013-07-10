(load "prereqs.scm")

(display "An interpreter for a language with call/cc")
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
    (if ((ceq? 'Y-cps) y) Y-cps
    (if ((ceq? '*) y) c*
    (if ((ceq? '+) y) c+
    (if ((ceq? 'add1) y) cadd1
    (if ((ceq? 'sub1) y) csub1
        error)))))))

(define empty-k (lambda (v) v))

(define call/cc-fun
  (lambda (k)
    (lambda (a)
      (lambda (k^)        
        (k a)))))

(define value-of
  (lambda (exp)
    (lambda (env)      
      (pmatch exp
        (,exp (guard (number? exp)) (lambda (k) (k exp)))
        (,exp (guard (symbol? exp)) (lambda (k) (k (env exp))))
        ((lambda (,x) ,body)
         (let ((sbody (value-of body)))           
           (let ((sfun (lambda (a)
                         (sbody
                          (lambda (y)
                            (if ((ceq? x) y) a (env y)))))))
             (lambda (k) (k sfun)))))          
        ((call/cc ,rator)
         (let ((srator ((value-of rator) env)))           
           (lambda (k)
             (((value-of rator) env)
              (lambda (p) ((p (lambda (a)
                           (lambda (k^)        
                             (k a)))) k))))))
        ((,rator ,rand)
         (let ((srator ((value-of rator) env)))
           (let ((srand ((value-of rand) env)))
             (lambda (k)
               (srator (lambda (p)
                         (srand (lambda (a)
                                  ((p a) k)))))))))))))

(define eval-exp
  (lambda (t)
    (((value-of t) init-env) empty-k)))

;;;;;;;;;;;;;;;;; Test Suite

(test-check "lambda-calc"
  (eval-exp '((lambda (x) ((lambda (y) x) 6)) 5))
  5)

(test-check "worse lambda calc"
  (eval-exp '(((lambda (x) (lambda (y) x)) 5) 6))
  5)

(test-check "test1"
  (eval-exp '(add1 (add1 (call/cc (lambda (k) (add1 (k (add1 (k 1)))))))))
  3)

(test-check "test2"
  (eval-exp '(add1 (add1 (call/cc (lambda (k) (add1 (k 1)))))))
  3)

(test-check "test3"  
  (eval-exp '(add1 (add1 (call/cc (lambda (k) (k (k (k (k (k (k 1)))))))))))
  3)

(test-check "test4"
  (eval-exp '(add1 ((+ 5) (call/cc (lambda (k) (k (k (k (k (k (k 1)))))))))))
  7)

(test-check "s&r"
  (eval-exp '((+ (call/cc (lambda (k) ((+ 10) (k 100))))) (call/cc (lambda (k^) 1))))
  101)

(test-check "working test1"
  (eval-exp '((+ (call/cc (lambda (k) ((+ 10) (k 100))))) (call/cc (lambda (k^) 1))))
  101)

(test-check "working-test2"
  (eval-exp '(add1 (add1 (call/cc (lambda (k) (k (add1 (k (k 5)))))))))
  7)

(test-check "working test3"
  (eval-exp '(add1 (call/cc (lambda (k) (k 5)))))
  6)

(test-check "other s&r"
  (eval-exp '(add1 ((+ (call/cc (lambda (k) ((+ 10) (k 100))))) (call/cc (lambda (k^) (add1 1))))))
  103)

(test-check "nested resets1"
  (eval-exp '(add1 (call/cc (lambda (k) (add1 (call/cc (lambda (k2) (k 5))))))))
  6)

(test-check "nested resets3"
  (eval-exp '(add1 (add1 (call/cc (lambda (k) (add1 (call/cc (lambda (k2) (k 5)))))))))
  7)

(test-check "nested resets4"
  (eval-exp '(add1 (add1 (call/cc (lambda (k) (add1 (call/cc (lambda (k2) (k2 (k 5))))))))))
  7)

(test-check "nested resets5"
  (eval-exp '(add1 (add1 (call/cc (lambda (k) (add1 (call/cc (lambda (k2) (k (k2 5))))))))))
  8)

(test-check "nested resets6"
  (eval-exp '(add1 (add1 (call/cc (lambda (k) (k (add1 (call/cc (lambda (k2) (k2 5))))))))))
  8)

(test-check "nested resets7"
  (eval-exp '(add1 (add1 (call/cc (lambda (k) (k (add1 (call/cc (lambda (k2) (k2 (k2 5)))))))))))
  8)

(test-check "plus and times"
  (eval-exp '((* 10) ((* 20) (call/cc (lambda (k) ((+ 5) (k ((+ 2) 3))))))))
  1000)
