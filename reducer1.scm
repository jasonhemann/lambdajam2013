(load "prereqs.scm")

(display "A suite of reducers to a variety of normal forms")
(newline)

;; lambda is disallowed as formal parameter. In order to support that,

;; We use gensym to guarantee uniqueness of variable names within exp.
;; Tested under Chez and Petite Chez Scheme. 

(define alpha-all
  (lambda (exp)
    (pmatch exp
      (,x (guard (symbol? x)) x)
      ((lambda (,x) ,body)
       (let ((g (gensym (symbol->string x))))
         `(lambda (,g) ,(subst g x (alpha-all body)))))
      ((,rator ,rand)
       `(,(alpha-all rator) ,(alpha-all rand))))))

(define reducer
  (lambda (under before)
    (lambda (str)      
      (letrec
        ((reducer (lambda (exp)
                 (pmatch exp
                   (,x (guard (symbol? x)) x)
                   ((lambda (,x) ,body)
                    `(lambda (,x) ,((under reducer) body)))
                   ((,rator ,rand)
                    (pmatch (reducer rator)
                      ((lambda (,x) ,body)
                       (str (subst ((before reducer) rand) x (alpha-all body))))
                      (,else `(,else ,((before reducer) rand)))))))))      
        reducer))))

(define yes (lambda (f) (lambda (x) (f x))))
(define no (lambda (f) (lambda (x) x)))

(define by-value (reducer no yes))
(define applicative (reducer yes yes))
(define head-spine (reducer yes no))
(define by-name (reducer no no))

(define ao-nf
  (letrec ((str (lambda (exp) ((applicative str) exp))))
    str))

(define bv-wnf
  (letrec ((str (lambda (exp) ((by-value str) exp))))
    str))

(define he-hnf
  (letrec ((str (lambda (exp) ((head-spine str) exp))))
    str))

(define bn-whnf
  (letrec ((str (lambda (exp) ((by-name str) exp))))
    str))

(define no-nf (applicative bn-whnf))
(define ha-nf (applicative bv-wnf))
(define hn-nf (applicative he-hnf))

(print-gensym 'pretty-suffix)

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
                     (lambda (f) (lambda (x) (f x)))))))
               (lambda (f) (lambda (x) (f (f (f (f (f x)))))))))))
    (((eval fn) add1) 0))
  1)

(test-check "test 1 ao-nf"
  (let ((fn (ao-nf '((lambda (x) (lambda (x) x)) z))))
    ((eval fn) 5))  
  5)

(test-check "test 5 ao-nf"
  (ao-nf '((lambda (x) y) z))
  'y)

(test-check "test 8 ao-nf"
  (ao-nf '((lambda (x) x) z))
  'z)

;; (test-check "complex-countdown ao-nf" 
;;   (let ((fn (ao-nf
;;               '(((lambda (w) (w w))
;;                  (lambda (f)
;;                    (lambda (n)
;;                      ((lambda (id)
;;                         ((n (lambda (_)
;;                               ((f f) (lambda (f)
;;                                        (lambda (x)
;;                                          (((n (lambda (g)
;;                                                 (lambda (h)
;;                                                   (h (g f)))))
;;                                            (lambda (u) x))
;;                                           id))))))
;;                          id))
;;                       (lambda (f) (lambda (x) (f x)))))))
;;                      (lambda (f) (lambda (x) (f (f (f (f (f x)))))))))))
;;     (((eval fn) add1) 0))
;;   1)

(test-check "test 1 bn-whnf"
  (let ((fn (bn-whnf '((lambda (x) (lambda (x) x)) z))))
    ((eval fn) 5))  
  5)

(test-check "test 5 bn-whnf"
  (bn-whnf '((lambda (x) y) z))
  'y)

(test-check "test 8 bn-whnf"
  (bn-whnf '((lambda (x) x) z))
  'z)

(test-check "complex-countdown bn-whnf"
  (let ((fn (bn-whnf
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
                      (lambda (f) (lambda (x) (f x)))))))
               (lambda (f) (lambda (x) (f (f (f (f (f x)))))))))))
    (((eval fn) add1) 0))
  1)

(test-check "test 1 he-hnf"
  (let ((fn (he-hnf
              '((lambda (x) (lambda (x) x)) z))))
    ((eval fn) 5))  
  5)

(test-check "test 5 he-hnf"
  (he-hnf '((lambda (x) y) z))
  'y)

(test-check "test 8 he-hnf"
  (he-hnf '((lambda (x) x) z))
  'z)

(test-check "complex-countdown he-hnf"
  (let ((fn (he-hnf
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
                      (lambda (f) (lambda (x) (f x)))))))
               (lambda (f) (lambda (x) (f (f (f (f (f x)))))))))))
    (((eval fn) add1) 0))
  1)