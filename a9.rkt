#lang scheme

(require "parenthec.rkt")

; GLOBAL REGISTERS
(define-registers k v env y expression lhs a)

; PROGRAM COUNTER
(define-program-counter pc)

(define-union expr
  (const cexp)
  (var n)
  (if test conseq alt)
  (mult nexp1 nexp2)
  (sub1 nexp)
  (zero nexp)
  (letcc body)
  (throw kexp vexp)
  (let exp body)              
  (lambda body)
  (app rator rand))

; CLOSURE CONSTRUCTOR
#;(define closure
  (lambda (body env)
   `(closure ,body ,env)))

(define-union clos
  (closure body^ env^))

(define-label apply-closure
  (union-case lhs clos
              [(closure body^ env^) (begin (set! k k) (set! env (envr_extend-env a env^)) (set! expression body^) (set! pc value-of-cps))]))

; ENVIRONMENT CONSTRUCTORS
#;(define extend-env
  (lambda (a^ env^)
    `(extend-env ,a^ ,env^)))

#;(define empty-env
  (lambda ()
    `(empty-env)))

(define-union envr
  (empty-env)
  (extend-env a^ env^))

(define-label apply-env
  (union-case env envr
      [(extend-env a^ env^) (if (zero? y) (begin (set! k k) (set! v a^) (set! pc apply-k)) (begin (set! env env^) (set! k k) (set! y (sub1 y)) (set! pc apply-env) ))] 
      [(empty-env) (error 'value-of-cps "unbound identifier")]))   

; CONTINUATION CONSTRUCTORS
#;(define inner-k-mult
  (lambda (v^ k^)
    `(inner-k-mult ,v^ ,k^)))

#;(define outer-k-mult
  (lambda (x2^ env^ k^)
    `(outer-k-mult ,x2^ ,env^ ,k^)))

#;(define inner-k-throw
  (lambda (v^)
    `(inner-k-throw ,v^)))

#;(define outer-k-throw
  (lambda (k-exp^ env^)
    `(outer-k-throw ,k-exp^ ,env^)))

#;(define inner-k-app
  (lambda (v^ k^)
    `(inner-k-app ,v^ ,k^)))

#;(define outer-k-app
  (lambda (rand^ env^ k^)
    `(outer-k-app ,rand^ ,env^ ,k^)))

#;(define constructor-sub1
  (lambda (k^)
    `(constructor-sub1 ,k^)))

#;(define constructor-zero
  (lambda (k^)
    `(constructor-zero ,k^)))

#;(define constructor-if
  (lambda (conseq^ alt^ env^ k^)
    `(constructor-if ,conseq^ ,alt^ ,env^ ,k^)))

#;(define constructor-let
  (lambda (body^ env^ k^)
    `(constructor-let ,body^ ,env^ ,k^)))

#;(define empty-k
  (lambda ()
    `(empty-k)))

(define-union kt
  (empty-k jumpout)
  (inner-k-mult v^ k^)
  (outer-k-mult x2^ env^ k^)
  (inner-k-throw v^)
  (outer-k-throw k-exp^ env^)
  (inner-k-app v^ k^)
  (outer-k-app rand^ env^ k^)
  (constructor-sub1 k^)
  (constructor-zero k^)
  (constructor-if conseq^ alt^ env^ k^)
  (constructor-let body^ env^ k^))

; INTERPRETER
(define-label value-of-cps
  (union-case expression expr
              [(const cexp) (begin (set! v cexp) (set! k k) (set! pc apply-k))]
              [(var n) (begin (set! env env) (set! y n) (set! k k) (set! pc apply-env))]
              [(if test conseq alt) (begin (set! k (kt_constructor-if conseq alt env k)) (set! env env) (set! expression test) (set! pc value-of-cps))]
              [(mult nexp1 nexp2) (begin (set! k (kt_outer-k-mult nexp2 env k)) (set! env env) (set! expression nexp1) (set! pc value-of-cps))]
              [(sub1 nexp) (begin (set! k (kt_constructor-sub1 k)) (set! env env) (set! expression nexp) (set! pc value-of-cps))]
              [(zero nexp) (begin (set! k (kt_constructor-zero k)) (set! env env) (set! expression nexp) (set! pc value-of-cps))]
              [(letcc body) (begin (set! k k) (set! env (envr_extend-env k env)) (set! expression body) (set! pc value-of-cps))]
              [(throw kexp vexp) (begin (set! k (kt_outer-k-throw vexp env)) (set! env env) (set! expression kexp) (set! pc value-of-cps))]
              [(let exp body) (begin (set! k (kt_constructor-let body env k)) (set! env env) (set! expression exp) (set! pc value-of-cps))]
              [(lambda body) (begin (set! v (clos_closure body env)) (set! k k) (set! pc apply-k))]
              [(app rator rand) (begin (set! k (kt_outer-k-app rand env k)) (set! env env) (set! expression rator) (set! pc value-of-cps))]))

(define-label apply-k
  (union-case k kt
              [(empty-k jumpout) (dismount-trampoline jumpout)]
              [(inner-k-mult v^ k^) (begin (set! v (* v v^)) (set! k k^) (set! pc apply-k))]
              [(outer-k-mult x2^ env^ k^) (begin (set! k (kt_inner-k-mult v k^)) (set! env env^) (set! expression x2^) (set! pc value-of-cps))]
              [(constructor-sub1 k^) (begin (set! v (sub1 v)) (set! k k^) (set! pc apply-k))]
              [(constructor-zero k^) (begin (set! v (zero? v)) (set! k k^) (set! pc apply-k))]
              [(constructor-if conseq^ alt^ env^ k^) (if v (begin (set! expression conseq^) (set! env env^) (set! k k^) (set! pc value-of-cps)) (begin (set! expression alt^) (set! env env^) (set! k k^) (set! pc value-of-cps)))]
              [(constructor-let body^ env^ k^) (begin (set! k k^) (set! env (envr_extend-env v env^)) (set! expression body^) (set! pc value-of-cps))]
              [(inner-k-throw v^) (begin (set! k v^) (set! v v) (set! pc apply-k))]
              [(outer-k-throw k-exp^ env^) (begin (set! k (kt_inner-k-throw v)) (set! env env^) (set! expression k-exp^) (set! pc value-of-cps))]
              [(inner-k-app v^ k^) (begin (set! lhs v^) (set! a v) (set! k k^) (set! pc apply-closure))]
              [(outer-k-app rand^ env^ k^) (begin (set! expression rand^) (set! env env^) (set! k (kt_inner-k-app v k^)) (set! pc value-of-cps))]))

; MAIN
(define-label main 
  (begin
    (set! expression (expr_let (expr_lambda (expr_lambda (expr_if (expr_zero (expr_var 0)) (expr_const 1) (expr_mult (expr_var 0) (expr_app (expr_app (expr_var 1) (expr_var 1)) (expr_sub1 (expr_var 0)))))))
           (expr_mult (expr_letcc (expr_app (expr_app (expr_var 1) (expr_var 1)) (expr_throw (expr_var 0) (expr_app (expr_app (expr_var 1) (expr_var 1)) (expr_const 4))))) (expr_const 5))))
    (set! env (envr_empty-env))
    (set! pc value-of-cps)
    (mount-trampoline kt_empty-k k pc)
    (printf "Fact 5: ~s\n" v)))

(main)

; (pc2c "interp.pc" "a9.c" "a9.h")
