#lang racket
(require "pmatch.rkt")

(define empty-env
  (lambda ()
    '()))

(define extend-env
  (lambda (id arg env)
    (cons (cons id arg) env)))

(define apply-env
  (lambda (env var)
    (cond
      [(assq var env) => cdr]
      [else (error 'env "unbound variable. ~s" var)])))

; side effects
(define b (box 5))

; new box
(set-box! (box (unbox b)) 2)

(unbox b)

(set-box! b 2)

(unbox b)

(define val-of
  (lambda (exp env)
    (pmatch exp
            [`,x (guard (number? x)) x]
            [`,x (guard (symbol? x)) (env x)]
            [`(lambda (,x) ,body)
             (lambda (a) (val-of body (lambda (y) (if (eq? x y) a (env y)))))]
            [`(,rator ,x) (guard (symbol? x)) ((val-of rator env) (env x))]
            [`(,rator ,rand) (guard (not (symbol? rand)))
                            ((val-of rator env) (val-of rand env))])))
; Call-By-Value
(define val-of-boxes
  (lambda (exp env)
    (pmatch exp
            [`,x (guard (number? x)) x]
            [`,x (guard (symbol? x)) (unbox (apply-env env x))]
            [`(lambda (,x) ,body)
             (lambda (a) (val-of-boxes body (extend-env x a env)))]
            [`(begin2 ,e1 ,e2) (begin (val-of-boxes e1 env) (val-of-boxes e2 env))]
            [`(set! ,x ,rhs) (set-box! (apply-env env x) (val-of-boxes rhs env))]
            [`(,rator ,x) (guard (symbol? x)) ((val-of-boxes rator env) (box (unbox (apply-env env x))))]
            [`(,rator ,rand) (guard (not (symbol? rand)))
                            ((val-of-boxes rator env) (box (val-of-boxes rand env)))])))
; Call-By-Reference
(define val-of-cbr
  (lambda (exp env)
    (pmatch exp
            [`,x (guard (number? x)) x]
            [`,x (guard (symbol? x)) (unbox (apply-env env x))]
            [`(lambda (,x) ,body)
             (lambda (a) (val-of-cbr body (extend-env x a env)))]
            [`(begin2 ,e1 ,e2) (begin (val-of-cbr e1 env) (val-of-cbr e2 env))]
            [`(set! ,x ,rhs) (guard (symbol? x)) (set-box! (apply-env env x) (val-of-cbr rhs env))]
            [`(,rator ,x) (guard (symbol? x)) ((val-of-cbr rator env) (apply-env env x))]
            [`(,rator ,rand) (guard (not (symbol? rand)))
                            ((val-of-cbr rator env) (box (val-of-cbr rand env)))])))
; Call-By-Name
(define val-of-cbname
  (lambda (exp env)
    (pmatch exp
            [`,x (guard (number? x)) x]
            [`,x (guard (symbol? x)) (unbox (apply-env env x))]
            [`(lambda (,x) ,body)
             (lambda (a) (val-of-cbname body (extend-env x a env)))]
            [`(,rator ,x) (guard (symbol? x)) ((val-of-cbname rator env) (apply-env env x))]
            [`(,rator ,rand) (guard (not (symbol? rand)))
                            ((val-of-cbname rator env) (box (lambda () (val-of-cbname rand env))))])))
; Call-By-Need
(define unbox/need
  (lambda (b)
    (let ([val ((unbox b))]) 
      (set-box! b (lambda () val)))))

(define val-of-cbneed
  (lambda (exp env)
    (pmatch exp
            [`,x (guard (number? x)) x]
            [`,x (guard (symbol? x)) (unbox/need (apply-env env x))]
            [`(lambda (,x) ,body)
             (lambda (a) (val-of-cbneed body (extend-env x a env)))]
            [`(begin2 ,e1 ,e2) (begin (val-of-cbneed e1 env) (val-of-cbneed e2 env))]
            [`(set! ,x ,rhs) (guard (symbol? x)) (set-box! (apply-env env x) (val-of-cbneed rhs env))]
            [`(,rator ,x) (guard (symbol? x)) ((val-of-cbneed rator env) (apply-env env x))]
            [`(,rator ,rand) (guard (not (symbol? rand)))
                            ((val-of-cbneed rator env) (box (val-of-cbneed rand env)))])))