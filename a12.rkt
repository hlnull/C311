#lang racket


(define-syntax do
  (syntax-rules (<-)
    ((_ bind e) e)
    ((_ bind (v <- e) e* e** ...)
     (bind e (lambda (v) (do bind e* e** ...))))
    ((_ bind e e* e** ...)
     (bind e (lambda (_) (do bind e* e** ...))))))


(define empty-k
  (lambda ()
    (let ((once-only #f))
      (lambda (v)
        (if once-only
	    (error 'empty-k "You can only invoke the empty continuation once")
            (begin (set! once-only #t) v))))))


; Maybe Monad
(define return-maybe
  (lambda (a) `(Just ,a)))


(define bind-maybe
  (lambda (ma f)
    (cond
      [(eq? (car ma) `Just) (f (cadr ma))]
      [(eq? (car ma) `Nothing) `(Nothing)])))


(define fail
  (lambda () `(Nothing)))


(define assv-maybe
  (lambda (v ls)
    (cond
      [(null? ls) (fail)]
      [else 
       (bind-maybe (return-maybe (car ls))
                   (lambda (s)
                     (if (eq? v (car s))
                         (return-maybe (cdr s))
                         (assv-maybe v (cdr ls)))))])))


(assv-maybe 'c '((a . 1) (b . 2) (c . 3)))
(assv-maybe 'd '((a . 1) (b . 2) (c . 3)))


; Writer Monad
(define return-writer
  (lambda (a)
    `(,a . ())))


(define bind-writer
  (lambda (ma f)
    (let ((mb (f (car ma))))
      `(,(car mb) . ,(append (cdr ma) (cdr mb))))))


(define tell-writer
  (lambda (to-writer)
    `(_. (,to-writer))))


(define partition-writer
  (lambda (pred ls)
    (cond
      [(null? ls) (return-writer `())]
      [(pred (car ls))
       (bind-writer
        (tell-writer (car ls))
        (lambda (_)
          (partition-writer pred (cdr ls))))]
      [else
       (bind-writer
        (partition-writer pred (cdr ls))
        (lambda (d)
          (return-writer
           (cons (car ls) d))))])))


(partition-writer odd? '(1 2 3 4 5 6 7 8 9 10))
(partition-writer even? '(1 2 3 4 5 6 7 8 9 10))


(define power
  (lambda (x n)
    (cond
      [(zero? n) 1]
      [(= n 1) x]
      [(odd? n) (* x (power x (sub1 n)))]
      [(even? n) (let ((nhalf (/ n 2)))
                   (let ((y (power x nhalf)))
                     (* y y)))])))

 
(define power-cps
  (lambda (x n k)
    (cond
      [(zero? n) (k 1)]
      [(= n 1) (k x)]
      [(odd? n) (power-cps x (sub1 n) (lambda (x^) (k (* x x^))))]
      [(even? n) (let ((nhalf (/ n 2)))
                   (power-cps x nhalf (lambda (x) (k (* x x)))))])))


(define powerXpartials
  (lambda (x n)
    (cond
      [(zero? n) (return-writer 1)]
      [(= n 1) (return-writer x)]
      [(odd? n) (bind-writer
                 (powerXpartials x (sub1 n))
                 (lambda (d)
                   (bind-writer (tell-writer d) (lambda (_) (return-writer (* x d))))))]
      [(even? n) (let ((nhalf (/ n 2)))
                   (bind-writer (powerXpartials x nhalf)
                                (lambda (d)
                                  (bind-writer (tell-writer d)
                                               (lambda (_) (return-writer (* d d)))))))])))


(power-cps 2 6 (empty-k))
(powerXpartials 2 6)

(power-cps 3 5 (empty-k))
(powerXpartials 3 5)

(power-cps 5 7 (empty-k))
(powerXpartials 5 7)


; State Monad
(define return-state
  (lambda (a)
    (lambda (s)
      `(,a . ,s))))


(define bind-state
  (lambda (ma f)
    (lambda (s)
      (let ((vs (ma s)))
        (let ((v (car vs))
              (s^ (cdr vs)))
          ((f v) s^))))))


(define get-state
   (lambda (s) `(,s . ,s)))


(define put-state
  (lambda (new-s)
    (lambda (s)
      `(_ . ,new-s))))


(define replace-with-count
  (lambda (x ls)
    (cond
      [(null? ls) (return-state `())]
      [(pair? (car ls))
       (do bind-state
         (a <- (replace-with-count x (car ls)))
         (d <- (replace-with-count x (cdr ls)))
         (return-state (cons a d)))]
      [(equal? x (car ls))
       (do bind-state
         (s <- get-state)
         (put-state (add1 s))
         (r <- (replace-with-count x (cdr ls)))
         (return-state (cons s r)))]  
      [else
       (do bind-state
         (r <- (replace-with-count x (cdr ls)))
         (return-state (cons (car ls) r)))])))


((replace-with-count 'o '(a o (t o (e o t ((n) o))))) 0)
((replace-with-count 'o '((h (i s o) a) o s (o e n))) 0)
((replace-with-count 'o '(o (h (o s o) o) o)) 0)


; Mixed Monads Problems
(define traverse
  (lambda (return bind f)
    (letrec
        ((trav
           (lambda (tree)
             (cond
               [(pair? tree)
                (do bind
                  (a <- (trav (car tree)))
                  (d <- (trav (cdr tree)))
                  (return (cons a d)))]
               [else (f tree)]))))
        trav)))


; using the maybe monad
(define reciprocal
  (lambda (n)
    (cond
      [(zero? n) (fail)]
      [else (return-maybe (/ 1 n))])))


(reciprocal 0)
(reciprocal 2)


(define traverse-reciprocal
  (traverse return-maybe bind-maybe reciprocal))


(traverse-reciprocal '((1 . 2) . (3 . (4 . 5))))
(traverse-reciprocal '((1 . 2) . (0 . (4 . 5))))


; using the writer monad
(define halve
  (lambda (n)
    (cond
      [(even? n) (return-writer (/ n 2))]
      [else
       (bind-writer
        (tell-writer n)
        (lambda (_)
          (return-writer n)))])))


(halve 6)
(halve 5)


(define traverse-halve
  (traverse return-writer bind-writer halve))


(traverse-halve '((1 . 2) . (3 . (4 . 5))))


; using the state monad
(define state/sum
  (lambda (n)
    (do bind-state
      (s <- get-state)
      (put-state (+ s n))
      (return-state s))))


((state/sum 5) 0)
((state/sum 2) 0)
((state/sum 2) 3)


(define traverse-state/sum
  (traverse return-state bind-state state/sum))


((traverse-state/sum '((1 . 2) . (3 . (4 . 5)))) 0)


; Continuation monad
(define return-cont
  (lambda (a)
    (lambda (k)
      (k a))))


(define bind-cont
  (lambda (ma f)
    (lambda (k)
      (let ((k^ (lambda (a)
                  (let ((mb (f a)))
                    (mb k)))))
        (ma k^)))))


(define callcc
  (lambda (g)
    (lambda (k)
      (let ((k-as-proc (lambda (a) (lambda (k^) (k a)))))
        (let ((ma (g k-as-proc)))
          (ma k))))))


; empty-env, apply-env, extend-env, closure, and apply-proc
(define apply-proc
  (lambda (lhs rhs)
    (lhs rhs)))


(define empty-env
  (lambda ()
    (lambda (var) (error "unbound variable  ̃s" var))))


(define extend-env
  (lambda (id arg env)
    (lambda (var)
      (if (eq? id var)
          arg
          (env var)))))


(define apply-env
  (lambda (env var)
    (env var)))


(define closure
  (lambda (id body env)
    (lambda (arg)
      (value-of-cps body (extend-env id arg env)))))


(define apply-closure
  (lambda (lhs rhs)
    (lhs rhs)))


(define value-of-cps
  (lambda (expr env)
    (match expr
      [(? number?) (return-cont expr)]
      [(? boolean?) (return-cont expr)]       
      [(? symbol?) (return-cont (apply-env env expr))]
      [`(* ,x1 ,x2) (bind-cont (value-of-cps x1 env)
                               (lambda (x)
                                 (bind-cont (value-of-cps x2 env)
                                            (lambda (x^)
                                              (return-cont (* x x^))))))]
      [`(sub1 ,x) (bind-cont (value-of-cps x env)
                             (lambda (x)
                               (return-cont(sub1 x))))]
      [`(zero? ,x) (bind-cont (value-of-cps x env)
                              (lambda (x)
                                (return-cont (zero? x))))]
      [`(if ,test ,conseq ,alt) (bind-cont (value-of-cps test env)
                                           (lambda (x)
                                             (if x
                                                 (value-of-cps conseq env)
                                                 (value-of-cps alt env))))]
      [`(capture ,k-id ,body) (callcc (lambda (k)
                                        (value-of-cps body (extend-env k-id k env))))]
      [`(return ,k-exp ,v-exp) (bind-cont (value-of-cps k-exp env)
                                          (lambda (x)
                                            (bind-cont (value-of-cps v-exp env)
                                                       (lambda (x^)
                                                         (x x^)))))]
      [`(lambda (,id) ,body) (return-cont (closure id body env))]
      [`(,rator ,rand) (bind-cont (value-of-cps rator env)
                                  (lambda (x)
                                    (bind-cont (value-of-cps rand env)
                                               (lambda (x^)
                                                 (apply-closure x x^)))))])))


(define fact-5
  '((lambda (f)
      ((f f) 5))
    (lambda (f)
      (lambda (n)
        (if (zero? n)
            1
            (* n ((f f) (sub1 n))))))))


((value-of-cps fact-5 (empty-env)) (lambda (v) v))


(define capture-fun
  '(* 3 (capture q (* 2 (return q 4)))))


((value-of-cps capture-fun (empty-env)) (lambda (v) v))