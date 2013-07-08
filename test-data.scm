

;; A complicated countdown from 5 to 0 which returns 1. It uses a
;; Church representation of numbers,pred, and if-zero?

(define complex-countdown
  '(((((lambda (w) (w w))
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
      (lambda (f) (lambda (x) (f (f (f (f (f x)))))))) add1) 0))