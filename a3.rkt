#lang racket
(require "pmatch.rkt")

; Part 1: Interpreters and Environments
(define value-of
  (lambda (exp env)
    (pmatch exp
            [`,x (guard (integer? x)) x]
            [`,x (guard (symbol? x)) (env x)]
            [`,x (guard (boolean? x)) x]
            [`(lambda (,id) , body)
                       (lambda (arg)
                         (value-of body (lambda (var)
                                          (if (eq? id var)
                                              arg
                                              (env var)))))]
            [`(sub1 ,op) (sub1 (value-of op env))]
            [`(zero? ,op) (zero? (value-of op env))]
            [`(* ,op1 ,op2) (* (value-of op1 env) (value-of op2 env))]
	    [`(let ((,a ,b)) ,body) (let ((c (value-of b env))) (value-of body (lambda (var)
                                                                                 (if (eq? var a) c (env var)))))]
            [`(if ,test ,conseq ,alt)
                               (if (value-of test env)
                                   (value-of conseq env)
                                   (value-of alt env))]
            [`(,rator ,rand) ((value-of rator env) (value-of rand env))])))

; functional (higher-order) representation of environments
(define empty-env-fn
  (lambda ()
    (lambda (y) (error 'value-of "unbound variable ~s" y))))

(define extend-env-fn
  (lambda (id arg env)
    (lambda (var)
      (if (eq? id var)
          arg
          (apply-env-fn env var)))))

(define apply-env-fn
  (lambda (env var)
    (env var)))

(define value-of-fn
  (lambda (exp env)
    (pmatch exp
            [`,n (guard (integer? n)) n]
            [`,var (guard (symbol? var)) (apply-env-fn env var)]
            [`,x (guard (boolean? x)) x]
            [`(lambda (,id) ,body)
             (lambda (arg)
               (value-of-fn body (extend-env-fn id arg env)))]
            [`(sub1 ,op) (sub1 (value-of-fn op env))]
            [`(zero? ,op) (zero? (value-of-fn op env))]
            [`(* ,op1 ,op2) (* (value-of-fn op1 env) (value-of-fn op2 env))]
	    [`(let ((,a ,b)) ,body) (let ((c (value-of-fn b env))) (value-of-fn body (extend-env-fn a c env)))]
            [`(if ,test ,conseq ,alt)
                               (if (value-of-fn test env)
                                   (value-of-fn conseq env)
                                   (value-of-fn alt env))]
            [`(,rator ,rand) ((value-of-fn rator env) (value-of-fn rand env))])))

; data-structural representation of environments
(define empty-env-ds
  (lambda ()
    `(empty-env-ds)))

(define extend-env-ds
  (lambda (id arg env)
    `(extend-env-ds ,id ,arg ,env)))

(define apply-env-ds
  (lambda (env var)
    (pmatch env
            [`(empty-env-ds) (lambda (y) (error 'value-of "unbound variable ~s" y))]
            [`(extend-env-ds ,id ,arg ,env)
             (if (eq? id var)
                 arg
                 (apply-env-ds env var))])))

(define value-of-ds
  (lambda (exp env)
    (pmatch exp
            [`,n (guard (integer? n)) n]
            [`,var (guard (symbol? var)) (apply-env-ds env var)]
            [`,x (guard (boolean? x)) x]
            [`(lambda (,id) ,body)
             (lambda (arg)
               (value-of-ds body (extend-env-ds id arg env)))]
            [`(sub1 ,op) (sub1 (value-of-ds op env))]
            [`(zero? ,op) (zero? (value-of-ds op env))]
            [`(* ,op1 ,op2) (* (value-of-ds op1 env) (value-of-ds op2 env))]
	    [`(let ((,a ,b)) ,body) (let ((c (value-of-ds b env))) (value-of-ds body (extend-env-ds a c env)))]
            [`(if ,test ,conseq ,alt)
                               (if (value-of-ds test env)
                                   (value-of-ds conseq env)
                                   (value-of-ds alt env))]
            [`(,rator ,rand) ((value-of-ds rator env) (value-of-ds rand env))])))

; Part 2 ''fo-eulav''
(define fo-eulav
  (lambda (expr env)
    (pmatch expr
            [`,n (guard (number? n)) n ]
            [`,y (guard (symbol? y)) (env y)]
            [`(,body (,x) adbmal) (lambda (z) (fo-eulav body (lambda (y)
                                                               (if (eqv? y x) z (env y)))))  ]
            [`(,body ((,b ,a)) tel) (let ((c (fo-eulav b env))) (fo-eulav body (lambda (y)
                                                                                 (if (eqv? y a) c (env y))))) ]
            [`(,false ,true ,cond fi) (if (fo-eulav cond env) (fo-eulav true env) (fo-eulav false env))  ]
	    [`(,arg ?orez) (zero? (fo-eulav arg env)) ]
	    [`(,num 1bus) (sub1 (fo-eulav num env))]
	    [`(,n2 ,n1 *) (* (fo-eulav n1 env) (fo-eulav n2 env))]
	    [`(,rand ,rator) ((fo-eulav rator env) (fo-eulav rand env)) ])))
; 7
(define c0 (lambda (f) (lambda (x) x)))
(define c5 (lambda (f) (lambda (x) (f (f (f (f (f x))))))))

(define c+ (lambda (m) 
               (lambda (n) 
                 (lambda (a) (lambda (b) ((m a) ((n a) b)))))))

(let ((c10 ((c+ c5) c5)))
    ((c10 add1) 0))

; Church predecessor