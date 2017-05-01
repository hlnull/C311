#lang racket
(require "pmatch.rkt")
;1 
(define (lex exp acc)
  (pmatch exp
          [`,x (guard (number? x)) `(const ,x)]
          [`,x (guard (symbol? x))
               (if (memv x acc)
                   `(var ,(- (length acc) (length (memv x acc))))
                   `())]
          [`(zero? ,op) `(zero? ,(lex op acc))]
          [`(sub1 ,op) `(sub1 ,(lex op acc))]
          [`(* ,op1 ,op2) `(* ,(lex op1 acc) ,(lex op2 acc))]
          [`(if ,test ,conseq ,alt)
           `(if ,(lex test acc) ,(lex conseq acc) ,(lex alt acc))]
          [`(let ((,a ,b)) ,body) `(let ,(lex b acc) ,(lex body (cons a acc)))]
          [`(lambda (,x) ,body)
           `(lambda ,(lex body (cons x acc)))]
          [`(,rator,rand)
           `(,(lex rator acc)
             ,(lex rand acc))]))
;2
; Environments As Untagged Lists
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

; a functional representation of closures
(define closure-fn
  (lambda (id body env)
    (lambda (arg)
      (value-of-fn body (extend-env id arg env)))))

(define apply-closure-fn
  (lambda (closure arg)
    (closure arg)))

(define value-of-fn
  (lambda (exp env)
    (pmatch exp
            [`,n (guard (integer? n)) n]
            [`,var (guard (symbol? var)) (apply-env env var)]
            [`,x (guard (boolean? x)) x]
            [`(lambda (,id) ,body) (closure-fn id body env)]
            [`(sub1 ,op) (sub1 (value-of-fn op env))]
            [`(zero? ,op) (zero? (value-of-fn op env))]
            [`(* ,op1 ,op2) (* (value-of-fn op1 env) (value-of-fn op2 env))]
	    [`(let ((,a ,b)) ,body) (let ((c (value-of-fn b env))) (value-of-fn body (extend-env a c env)))]
            [`(if ,test ,conseq ,alt)
                               (if (value-of-fn test env)
                                   (value-of-fn conseq env)
                                   (value-of-fn alt env))]
            [`(,rator ,rand) (apply-closure-fn (value-of-fn rator env) (value-of-fn rand env))])))

; a data-structural representation of closures
(define closure-ds
  (lambda (id body env)
    `(closure ,id ,body ,env)))

(define apply-closure-ds
  (lambda (closure arg)
    (pmatch closure
      [`(closure ,id ,body ,env)
       (value-of-ds body (extend-env id arg env))])))

(define value-of-ds
  (lambda (exp env)
    (pmatch exp
            [`,n (guard (integer? n)) n]
            [`,var (guard (symbol? var)) (apply-env env var)]
            [`,x (guard (boolean? x)) x]
            [`(lambda (,id) ,body) (closure-ds id body env)]
            [`(sub1 ,op) (sub1 (value-of-ds op env))]
            [`(zero? ,op) (zero? (value-of-ds op env))]
            [`(* ,op1 ,op2) (* (value-of-ds op1 env) (value-of-ds op2 env))]
	    [`(let ((,a ,b)) ,body) (let ((c (value-of-ds b env))) (value-of-ds body (extend-env a c env)))]
            [`(if ,test ,conseq ,alt)
                               (if (value-of-ds test env)
                                   (value-of-ds conseq env)
                                   (value-of-ds alt env))]
            [`(,rator ,rand) (apply-closure-ds (value-of-ds rator env) (value-of-ds rand env))])))

; dynamic scope
(define value-of-dynamic
  (lambda (exp env)
    (match exp
      ;;null
      [`(null? ,a) (null? (value-of-dynamic a env))]
      
      ;;cons 
      [`(cons ,a ,b) (cons (value-of-dynamic a env) (value-of-dynamic b env))]
      
      ;;car 
      [`(car ,ls) (car (value-of-dynamic ls env))]
      
      ;;cdr
      [`(cdr ,ls) (cdr (value-of-dynamic ls env))]
      
      ;;zero 
      [`(zero? ,a) (zero? (value-of-dynamic a env))]
      
      ;;sub1
      [`(sub1 ,a) (sub1 (value-of-dynamic a env))]
      
      ;;multiply *
      [`(* ,a ,b) (* (value-of-dynamic a env) (value-of-dynamic b env))]
      
      ;;let - hide
      [`(let ([,var ,exp]) ,body) (value-of-dynamic body (extend-env var (value-of-dynamic exp env) env))]
      
      ;;quote
      [`(quote ,v) v]
      
      ;;number
      [`,n #:when (number? n) n]

      ;;variable
      [`,var #:when (symbol? var) (apply-env env var )]

      ;;lambda abstraction 
      [`(lambda (,id) ,body) `(lambda (,id) ,body)]

      ;;application
      [`(,rator ,rand) (match-let ([`(lambda (,x) ,b) (value-of-dynamic rator env)]
                                   [`,a (value-of-dynamic rand env)])
                         (value-of-dynamic b (extend-env x a env)))]
      ;;if condition
      [`(if ,test ,conseq ,alt) (if (value-of-dynamic test env) (value-of-dynamic conseq env) (value-of-dynamic alt env))])))

(value-of-fn
   '(let ([x 2])
      (let ([x 1])
      ((lambda (y) (* x y)) 2)))
   (empty-env))


(value-of-dynamic
   '(let ([x 2])
      (let ([x 1])
      ((lambda (y) (* x y)) 2)))
   (empty-env))
