(load "objects.scm")

(define base-env
  (lambda ()
    '()))

(define extend-env
  (lambda (id a env)
    (cons (cons id a) env)))

(define extend-env*
  (lambda (id* a* env)
    (cond
      [(null? id*) env]
      [else (extend-env (car id*) (car a*)
              (extend-env* (cdr id*) (cdr a*) env))])))

(define apply-env
  (lambda (env id)
    (cond
      [(assq id env) => cdr]
      [else (error 'apply-env "Unbound variable: ~s" id)])))

(define closure
  (lambda (id body env)
    `(closure ,id ,body ,env)))

(define apply-proc
  (lambda (p a)
    (pmatch p
      [(closure ,id ,body ,env) 
       (value-of body (extend-env id (box a) env))])))

(define value-of-expr
  (lambda (expr env)
    (pmatch expr
      [,n (guard (or (number? n) (boolean? n))) n]
      [,x (guard (symbol? x)) (unbox (apply-env env x))]
      [(+ ,x1 ,x2)  (+ (value-of x1 env) (value-of x2 env))]      
      [(- ,x1 ,x2)  (- (value-of x1 env) (value-of x2 env))]
      [(* ,x1 ,x2)  (* (value-of x1 env) (value-of x2 env))]      
      [(cons ,a ,d)  (cons (value-of a env) (value-of d env))]
      [(sub1 ,x)  (sub1 (value-of x env))]
      [(print ,e)  (printf ";~s~n" (value-of e env))]
      [(zero? ,x)  (zero? (value-of x env))]
      [(if ,e0 ,e1 ,e2)
       (if (value-of e0 env) (value-of e1 env) (value-of e2 env))]
      [(lambda (,id) ,body)  (closure id body env)]
      [(let ([,id ,e]) ,body)  (value-of `((lambda (,id) ,body) ,e) env)]
      [(set! ,id ,e)  (set-box! (apply-env env id) (value-of e env))]
      [(begin ,e . ,e*)
       (let loop ((e e) (e* e*))
         (if (null? e*) (value-of e env)
           (begin (value-of e env) (loop (car e*) (cdr e*)))))]
      [(,rator ,rand)  (apply-proc (value-of rator env) (value-of rand env))]
      [else (error 'value-of "Unmatched expression ~s" expr)])))

(define base-f-names '())
(define base-m*
  `((initialize ,(value-of '(lambda (z) 0) (base-env)) ,base-f-names)))
(define c* `((object ,base-f-names ,base-m*)))

(define-syntax test-check
  (syntax-rules ()
    ((_ title tested-expression expected-result)
     (let* ((expected expected-result)
            (produced tested-expression))
       (if (equal? expected produced)
         (printf ";~s works!\n" title)
         (error 'test-check
           "Failed ~s: ~a~%Expected: ~a~%Computed: ~a"
           title 'tested-expression expected produced))))))

;--------------------------------------------------------------------------
(define fact-5
  '(let ([f (lambda (f)
              (lambda (n)
                (if (zero? n) 1 (* n ((f f) (sub1 n))))))])
     ((f f) 5)))

(test-check "fact-5"
  (value-of fact-5 (base-env))
  120)

(test-check "oo-fact-5"
  (value-of-program
    '([c1 object (u)
       ([fact n (if (zero? n) 1 (* n (send self fact (sub1 n))))])])
    '(let ((o1 (new c1 1000)))
       (send o1 fact 5)))
  120)

(test-check "oo-1"
  (value-of-program
    '([c1 object (i j)
       ([initialize x
          (begin
            (set! i x)
            (set! j (- 0 x)))]
        [countup d
          (begin
            (set! i (+ i d))
            (set! j (- j d)))]
        [get-state ignore
          (begin
            (send self printsomething 5000)
            (cons i j))]
        [printsomething x
          (print x)])])
    '(let ([t1 0])
       (let ([t2 0])
         (let ([o1 (new c1 3)])
           (begin
             (set! t1 (send o1 get-state 0))
             (send o1 countup 2)
             (set! t2 (send o1 get-state 0))
             (cons t1 t2))))))
  '((3 . -3) . (5 . -5)))

(test-check "oo-2"
  (value-of-program
    '([c1 object (x y)
       ([initialize w
          (begin
            (print (cons 10 (cons x y)))
            (set! x 11)
            (set! y 12)
            (print (cons -10 (cons x y))))]
        [m1 ignore
          (begin
            (print (cons 11 (cons x y)))
            (set! x (+ x 5))
            (set! y (- y 5))
            (print (cons -11 (cons x y))))]
        [m2 ignore
          (begin
            (print (cons 12 (cons x y)))
            (print (cons -12 (cons x y))))]
        [m3 ignore
          (begin
            (print ignore)
            (print (cons 13 (cons x y)))
            (print 10000)
            (print (cons -13 (cons x y))))])]
      [c2 c1 (y)
       ([initialize w
          (begin
            (print (cons 20 (cons x y)))
            (super initialize 121)
            (set! y 122)
            (print (cons -20 (cons x y))))]
        [m1 ignore
          (begin
            (print (cons 21 (cons x y)))
            (set! x (+ x 55))
            (set! y (- y 55))
            (print (cons -21 (cons x y)))
            (cons x y))]
        [m3 ignore
          (begin
            (print (cons 23 (cons x y)))
            (set! x (+ x 66))
            (set! y (- y 66))
            (print (cons -23 (cons x y))))])]
      [c3 c2 (x z)
       ([initialize w
          (begin
            (print (cons 30 (cons x (cons y z))))
            (super initialize 1121)
            (set! x 1131)
            (set! z 1132)
            (print (cons -30 (cons x (cons y z)))))]
        [m3 ignore
          (begin
            (print (cons 33 (cons x (cons y z))))
            (set! x (+ x 2131))
            (set! y (- y 2132))
            (set! z (- z 2133))
            (print (cons -33 (cons x (cons y z)))))])])
    '(let ([o3 (new c3 0)])
       (send o3 m1 7)))
  '(66 . 67))

;"fact-5" works!
;"oo-fact-5" works!
;5000
;5000
;"oo-1" works!
;(30 #<void> #<void> . #<void>)
;(20 #<void> . #<void>)
;(10 #<void> . #<void>)
;(-10 11 . 12)
;(-20 11 . 122)
;(-30 1131 122 . 1132)
;(21 11 . 122)
;(-21 66 . 67)
;"oo-2" works!
