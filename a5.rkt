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

(define closure-cbv
  (lambda (id body env)
    (lambda (arg)
      (val-of-cbv body (extend-env id arg env)))))

(define closure-cbr
  (lambda (id body env)
    (lambda (arg)
      (val-of-cbr body (extend-env id arg env)))))

(define closure-cbname
  (lambda (id body env)
    (lambda (arg)
      (val-of-cbname body (extend-env id arg env)))))

(define closure-cbneed
  (lambda (id body env)
    (lambda (arg)
      (val-of-cbneed body (extend-env id arg env)))))

(define apply-closure
  (lambda (closure arg)
    (closure arg)))

; call-by-value
(define val-of-cbv
  (lambda (exp env)
    (pmatch exp
            [`,x (guard (number? x)) x]
            [`,x (guard (boolean? x)) x]
            [`,x (guard (symbol? x)) (unbox (apply-env env x))]
            [`(zero? ,x) (zero? (val-of-cbv x env))]
            [`(sub1 ,x) (sub1 (val-of-cbv x env))]
            [`(* ,n1 ,n2) (* (val-of-cbv n1 env) val-of-cbv n2 env)]
            [`(if ,test ,conseq ,alt) (if (val-of-cbv test env)
                                          (val-of-cbv conseq env)
                                          (val-of-cbv alt env))]
            [`(random ,n) (random (val-of-cbv n env))]
            [`(lambda (,x) ,body) (closure-cbv x body env)]
            [`(begin2 ,e1 ,e2) (begin (val-of-cbv e1 env) (val-of-cbv e2 env))]
            [`(set! ,x ,rhs) (set-box! (apply-env env x) (val-of-cbv rhs env))]
            [`(,rator ,x) (guard (symbol? x)) (apply-closure (val-of-cbv rator env) (box (unbox (apply-env env x))))]
            [`(,rator ,rand) (guard (not (symbol? rand)))
                            (apply-closure (val-of-cbv rator env) (box (val-of-cbv rand env)))])))
; call-by-reference
(define val-of-cbr
  (lambda (exp env)
    (pmatch exp
            [`,x (guard (number? x)) x]
            [`,x (guard (boolean? x)) x]
            [`,x (guard (symbol? x)) (unbox (apply-env env x))]
            [`(zero? ,x) (zero? (val-of-cbr x env))]
            [`(sub1 ,x) (sub1 (val-of-cbr x env))]
            [`(* ,n1 ,n2) (* (val-of-cbr n1 env) val-of-cbr n2 env)]
            [`(if ,test ,conseq ,alt) (if (val-of-cbr test env)
                                          (val-of-cbr conseq env)
                                          (val-of-cbr alt env))]
            [`(random ,n) (random (val-of-cbr n env))]
            [`(lambda (,x) ,body) (closure-cbr x body env)]
            [`(begin2 ,e1 ,e2) (begin (val-of-cbr e1 env) (val-of-cbr e2 env))]
            [`(set! ,x ,rhs) (set-box! (apply-env env x) (val-of-cbr rhs env))]
            [`(,rator ,x) (guard (symbol? x)) (apply-closure (val-of-cbr rator env) (apply-env env x))]
            [`(,rator ,rand) (guard (not (symbol? rand)))
                            (apply-closure (val-of-cbr  rator env) (box (val-of-cbr rand env)))])))
; call-by-name
(define val-of-cbname
  (lambda (exp env)
    (pmatch exp
      [`,b (guard (boolean? b)) b]
      [`,n (guard (number? n)) n]
      [`,x (guard (symbol? x)) ((unbox (apply-env env x)))]
      [`(zero? ,n) (zero? (val-of-cbname n env))]
      [`(sub1 ,n) (sub1 (val-of-cbname n env))]
      [`(* ,n1 ,n2) (* (val-of-cbname n1 env) (val-of-cbname n2 env))]
      [`(if ,test ,conseq ,alt) (if (val-of-cbname test env)
                                    (val-of-cbname conseq env)
                                    (val-of-cbname alt env))]
      [`(begin2 ,e1 ,e2) (begin (val-of-cbname e1 env) (val-of-cbname e2 env))]
      [`(random ,n) (random (val-of-cbname n env))]
      [`(lambda (,x) ,body) (closure-cbname x body env)]
      [`(,rator ,y) (guard (symbol? y)) (apply-closure (val-of-cbname rator env) (apply-env env y))]
      [`(,rator ,rand) (apply-closure (val-of-cbname rator env)
                                      (box (lambda () (val-of-cbname rand env))))])))
val-of-cbname
; call-by-need
(define unbox/need
  (lambda (b)
    (let ([val ((unbox b))]) 
      (set-box! b (lambda () val)))))

(define val-of-cbneed
  (lambda (exp env)
    (pmatch exp
      [`,b (guard (boolean? b)) b]
      [`,n (guard (number? n)) n]
      [`,x (guard (symbol? x)) ((unbox (apply-env env x)))]
      [`(zero? ,n) (zero? (val-of-cbneed n env))]
      [`(sub1 ,n) (sub1 (val-of-cbneed n env))]
      [`(* ,n1 ,n2) (* (val-of-cbneed n1 env) (val-of-cbneed n2 env))]
      [`(if ,test ,conseq ,alt) (if (val-of-cbneed test env)
                                    (val-of-cbneed conseq env)
                                    (val-of-cbneed alt env))]
      [`(begin2 ,e1 ,e2) (begin (val-of-cbneed e1 env) (val-of-cbneed e2 env))]
      [`(random ,n) (random (val-of-cbneed n env))]
      [`(lambda (,x) ,body) (closure-cbneed x body env)]
      [`(,rator ,y) (guard (symbol? y)) (apply-closure (val-of-cbneed rator env) (apply-env env y))]
      [`(,rator ,rand) (apply-closure (val-of-cbneed rator env)
                                      (box (lambda () (val-of-cbneed rand env))))])))

(define random-sieve
    '((lambda (n)
        (if (zero? n)
            (if (zero? n) (if (zero? n) (if (zero? n) (if (zero? n) (if (zero? n) (if (zero? n) #t #f) #f) #f) #f) #f) #f)
            (if (zero? n) #f (if (zero? n) #f (if (zero? n) #f (if (zero? n) #f (if (zero? n) #f (if (zero? n) #f #t))))))))
      (random 2)))

(val-of-cbname random-sieve (empty-env))

(val-of-cbneed random-sieve (empty-env))

(val-of-cbname
   '((lambda (z) 100)
     ((lambda (x) (x x)) (lambda (x) (x x))))
   (empty-env))