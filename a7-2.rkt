#lang Racket
(require test-engine/racket-tests)

(define last-non-zero
  (lambda (ls)
    (let/cc k
      (letrec
	((last-non-zero
	   (lambda (ls)
	     (cond
               [(null? ls) '()]
               [(zero? (car ls)) (k (last-non-zero (cdr ls)))]
               [else (cons (car ls) (last-non-zero (cdr ls)))]
  	       ))))
	(last-non-zero ls)))))

(last-non-zero '(0))
(last-non-zero '(1 2 3 0 4 5))
(last-non-zero '(1 0 2 3 0 4 5))
(last-non-zero '(1 2 3 4 5))

; Part III: The interpreter
; Regular
(define value-of
  (lambda (expr env)
    (match expr
      [`(const ,expr) expr]
      [`(mult ,x1 ,x2) (* (value-of x1 env) (value-of x2 env))]
      [`(sub1 ,x) (sub1 (value-of x env))]
      [`(zero ,x) (zero? (value-of x env))]
      [`(if ,test ,conseq ,alt) (if (value-of test env) (value-of conseq env) (value-of alt env))]
      [`(let/cc ,body) (let/cc k (value-of body (lambda (y) (if (zero? y) k (env (sub1 y))))))]
      [`(throw ,k-exp ,v-exp) ((value-of k-exp env) (value-of v-exp env))]
      [`(let ,e ,body) (let ((a (value-of e env))) (value-of body (lambda (y) (if (zero? y) a (env (sub1 y))))))]
      [`(var ,expr) (env expr)]
      [`(lambda ,body) (lambda (a) (value-of body (lambda (y) (if (zero? y) a (env (sub1 y))))))]
      [`(app ,rator ,rand) ((value-of rator env) (value-of rand env))])))

; Continuation-Passing Style
(define empty-k
  (lambda ()
    `(empty-k)))

(define extend-env
  (lambda (a env)
    `(extend-env ,a ,env)))

(define empty-env
  (lambda ()
    `(empty-env)))

(define apply-env
  (lambda (env y k) 
    (match env
      [`(extend-env ,a^ ,env^) (if (zero? y) (apply-k k a^) (apply-env env^ (sub1 y) k))] 
      [`(empty-env) (error 'value-of "unbound identifier")])))

(define closure
  (lambda (body env)
   `(closure ,body ,env)))

(define apply-closure
  (lambda (lhs a k)
   (match lhs
     [`(closure ,body ,env) (value-of-cps body (extend-env a env) k)])))

(define value-of-cps
  (lambda (expr env k)
    (match expr
      [`(var ,expr) (apply-env env expr k)]
      [`(const ,expr) (apply-k k expr)]
      [`(mult ,x1 ,x2) (value-of-cps x1 env (outer-k-mult x2 env k))]
      [`(sub1 ,x) (value-of-cps x env (constructor-sub1 k))]
      [`(zero ,x) (value-of-cps x env (constructor-zero k))]
      [`(if ,test ,conseq ,alt) (value-of-cps test env (constructor-if conseq alt env k))]
      [`(let ,e ,body) (value-of-cps e env (constructor-let body env k))]
      [`(let/cc ,body) (value-of-cps body (extend-env k env) k)]
      [`(throw ,k-exp ,v-exp) (value-of-cps k-exp env (outer-k-throw v-exp env))]
      [`(lambda ,body) (apply-k k (closure body env))]
      [`(app ,rator ,rand) (value-of-cps rator env (outer-k-app rand env k))])))

(define apply-k
  (lambda (k v)
    (match k
      [`(empty-k) v]
      [`(inner-k-mult ,v^ ,k^) (apply-k k^ (* v v^))]
      [`(outer-k-mult ,x2^ ,env^ ,k^) (value-of-cps x2^ env^ (inner-k-mult v k^))]
      [`(constructor-sub1 ,k^) (apply-k k^ (sub1 v))]
      [`(constructor-zero ,k^) (apply-k k^ (zero? v))]
      [`(constructor-if ,conseq^ ,alt^ ,env^ ,k^) (if v (value-of-cps conseq^ env^ k^) (value-of-cps alt^ env^ k^))]
      [`(constructor-let ,body^ ,env^ ,k^) (value-of-cps body^ (extend-env v env^) k^)]
      [`(inner-k-throw ,v^) (apply-k v^ v)]
      [`(outer-k-throw ,k-exp^ ,env^) (value-of-cps k-exp^ env^ (inner-k-throw v))]
      [`(inner-k-app ,v^ ,k^) (apply-closure v^ v k^)]
      [`(outer-k-app ,rand^ ,env^ ,k^) (value-of-cps rand^ env^ (inner-k-app v k^))])))

(define inner-k-mult
  (lambda (v^ k^)
    `(inner-k-mult ,v^ ,k^)))

(define outer-k-mult
  (lambda (x2^ env^ k^)
    `(outer-k-mult ,x2^ ,env^ ,k^)))

(define inner-k-throw
  (lambda (v^)
    `(inner-k-throw ,v^)))

(define outer-k-throw
  (lambda (k-exp^ env^)
    `(outer-k-throw ,k-exp^ ,env^)))

(define inner-k-app
  (lambda (v^ k^)
    `(inner-k-app ,v^ ,k^)))

(define outer-k-app
  (lambda (rand^ env^ k^)
    `(outer-k-app ,rand^ ,env^ ,k^)))

(define constructor-sub1
  (lambda (k^)
    `(constructor-sub1 ,k^)))

(define constructor-zero
  (lambda (k^)
    `(constructor-zero ,k^)))

(define constructor-if
  (lambda (conseq^ alt^ env^ k^)
    `(constructor-if ,conseq^ ,alt^ ,env^ ,k^)))

(define constructor-let
  (lambda (body^ env^ k^)
    `(constructor-let ,body^ ,env^ ,k^)))

; Tests
(check-expect (value-of-cps '(const 5) (empty-env) (empty-k)) 5)
(check-expect (value-of-cps '(const 5) (empty-env) (empty-k)) 5)
(check-expect (value-of-cps '(mult (const 5) (const 5)) (empty-env) (empty-k)) 25)
(check-expect (value-of-cps '(zero (const 5)) (empty-env) (empty-k)) #f)
(check-expect (value-of-cps '(sub1 (const 5)) (empty-env) (empty-k)) 4)
(check-expect (value-of-cps '(sub1 (sub1 (const 5))) (empty-env) (empty-k)) 3)
(check-expect (value-of-cps '(zero (sub1 (const 6))) (empty-env) (empty-k)) #f)
(check-expect (value-of-cps '(if (zero (const 5)) (const 3) (mult (const 2) (const 2))) (empty-env) (empty-k)) 4)
(check-expect (value-of-cps '(if (zero (const 0)) (mult (const 2) (const 2)) (const 3)) (empty-env) (empty-k)) 4)
(check-expect (value-of-cps '(app (lambda (const 5)) (const 6)) (empty-env) (empty-k)) 5)
(check-expect (value-of-cps '(app (lambda (var 0)) (const 5)) (empty-env) (empty-k)) 5)
(check-expect (value-of-cps '(app (app (lambda (lambda (var 1))) (const 6)) (const 5)) (empty-env) (empty-k)) 6)
(check-expect (value-of-cps '(app (lambda (app (lambda (var 1)) (const 6))) (const 5)) (empty-env) (empty-k)) 5)
(check-expect (value-of-cps '(app (lambda (if (zero (var 0)) (const 4) (const 5))) (const 3)) (empty-env) (empty-k)) 5)
(check-expect (value-of-cps '(let (const 6) (const 4)) (empty-env) (empty-k)) 4)
(check-expect (value-of-cps '(let (const 5) (var 0)) (empty-env) (empty-k)) 5)
(check-expect (value-of-cps '(mult (const 5) (let (const 5) (var 0))) (empty-env) (empty-k)) 25)
(check-expect (value-of-cps '(app (if (zero (const 4)) (lambda (var 0)) (lambda (const 5))) (const 3)) (empty-env) (empty-k)) 5)
(check-expect (value-of-cps '(app (if (zero (const 0)) (lambda (var 0)) (lambda (const 5))) (const 3)) (empty-env) (empty-k)) 3)
(check-expect (value-of-cps '(let/cc (const 5)) (empty-env) (empty-k)) 5)
(check-expect (value-of-cps '(let/cc (throw (var 0) (const 5))) (empty-env) (empty-k)) 5)
(check-expect (value-of-cps '(let/cc (throw (var 0) (mult (const 5) (const 5)))) (empty-env) (empty-k)) 25)
(check-expect (value-of-cps '(let/cc (throw (app (lambda (var 0)) (var 0)) (mult (const 5) (const 5)))) (empty-env) (empty-k)) 25)
(check-expect (value-of-cps '(let/cc (sub1 (throw (var 0) (const 5)))) (empty-env) (empty-k)) 5)
(check-expect (value-of-cps '(let/cc (throw (throw (var 0) (const 5)) (const 6))) (empty-env) (empty-k)) 5)
(check-expect (value-of-cps '(let/cc (throw (const 5) (throw (var 0) (const 5)))) (empty-env) (empty-k)) 5)
(check-expect (value-of-cps '(mult (const 3) (let/cc (throw (const 5) (throw (var 0) (const 5))))) (empty-env) (empty-k)) 15)
(check-expect (value-of-cps '(if (zero (const 5)) (app (lambda (app (var 0) (var 0))) (lambda (app (var 0) (var 0)))) (const 4))
                       (empty-env)
                       (empty-k))
         4)
(check-expect (value-of-cps '(if (zero (const 0)) (const 4) (app (lambda (app (var 0) (var 0))) (lambda (app (var 0) (var 0)))))
                       (empty-env)
                       (empty-k))
         4)
(check-expect (value-of-cps '(app (lambda (app (app (var 0) (var 0)) (const 2)))
                             (lambda
                               (lambda 
                                 (if (zero (var 0))  
                                     (const 1)
                                     (app (app (var 1) (var 1)) (sub1 (var 0)))))))
                       (empty-env)
                       (empty-k))
         1)

(test)