; Prerequisites for the interpreters session

(define pwv
  (lambda (fl)
    (lambda (n)
      (display fl)
      (display " ")
      (display n)
      (newline)
      n)))

(define c+
  (lambda (x)
    (lambda (y)
      (+ x y))))

(define c*
  (lambda (x)
    (lambda (y)
      (* x y))))

(define ceq?
  (lambda (x)
    (lambda (y)
      (eq? x y))))

(define c=
  (lambda (x)
    (lambda (y)
      (= x y))))

(define Y
  (lambda (vo)
    ((lambda (f) (f f))
     (lambda (f) (vo (lambda (x) ((f f) x)))))))

(define cerror
  (lambda (y)
    (error 'cerror "unbound variable" y)))

; A simple testing utility

(define-syntax test-check
  (syntax-rules ()
    ((_ title tested-expression expected-result)
     (begin
       (printf "Testing ~s\n" title)
       (let* ((expected expected-result)
              (produced tested-expression))
         (or (equal? expected produced)
             (errorf 'test-check
               "Failed: ~a~%Expected: ~a~%Computed: ~a~%"
               'tested-expression expected produced)))))))

; A simple linear pattern matcher
; It is efficient (generates code at macro-expansion time) and simple:
; it should work on any R5RS (and R6RS) Scheme system.

;; Originally due to Oleg Kiselyov
;; Modified by Dan Friedman and Adam Foltzer, among others.

; (pmatch exp (quote name) <clause> ...[<else-clause>]) | ADDED (spring, 2013)
; (pmatch exp <clause> ...[<else-clause>]) |
; (pmatch exp name <clause> ...[<else-clause>]))          ADDED (summer, 2012)
; <clause> ::= (<pattern> <guard> exp ...)
; <else-clause> ::= (else exp ...)
; <guard> ::= boolean exp | ()
; <pattern> :: =
;        ,var  -- matches always and binds the var
;                 pattern must be linear! No check is done
;         _    -- matches always
;         ?    -- matches always                        REMOVED (August 28, 2012)
;        'exp  -- comparison with exp (using equal?)    REMOVED (August 8, 2012)
;        exp   -- comparison with exp (using equal?)
;        (<pattern1> <pattern2> ...) -- matches the list of patterns
;        (<pattern1> . <pattern2>)  -- ditto
;        ()    -- matches the empty list                REMOVED (August 28, 2012)

(define-syntax pmatch
  (syntax-rules (else guard)
    ((_ v (e ...) ...)
     (pmatch-aux #f v (e ...) ...))
    ((_ v name (e ...) ...)
     (pmatch-aux name v (e ...) ...))))

(define-syntax pmatch-aux
  (syntax-rules (else guard)
    ((_ name (rator rand ...) cs ...)
     (let ((v (rator rand ...)))
       (pmatch-aux name v cs ...)))
    ((_ name v)
     (begin
       (if 'name
           (printf "pmatch ~s failed\n" 'name)
           (printf "pmatch failed\n"))
       (printf "with input evaluating to ~s\n" v)
       (error 'pmatch "match failed")))
    ((_ name v (else e0 e ...)) (begin e0 e ...))
    ((_ name v (pat (guard g ...) e0 e ...) cs ...)
     (let ((fk (lambda () (pmatch-aux name v cs ...))))
       (ppat v pat (if (and g ...) (begin e0 e ...) (fk)) (fk))))
    ((_ name v (pat e0 e ...) cs ...)
     (let ((fk (lambda () (pmatch-aux name v cs ...))))
       (ppat v pat (begin e0 e ...) (fk))))))

(define-syntax ppat
  (syntax-rules (unquote)
    ((_ v (unquote var) kt kf) (let ((var v)) kt))
    ((_ v (x . y) kt kf)
     (if (pair? v)
       (let ((vx (car v)) (vy (cdr v)))
	 (ppat vx x (ppat vy y kt kf) kf))
       kf))
    ((_ v lit kt kf) (if (equal? v (quote lit)) kt kf))))
