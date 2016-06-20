#lang racket
(require "mk.rkt")
(require test-engine/racket-tests)

(define apply-Go
  (lambda (G e t)
    (fresh (a G^)
      (== `(,a . ,G^) G)
      (fresh (aa da)
        (== `(,aa . ,da) a)
        (conde
          ((== aa e) (== da t))
          ((=/= aa e) (apply-Go G^ e t)))))))

(define !-
  (lambda (G e t) 
    (conde

     ((numbero e) (== 'Nat t))

     ((== t 'Bool)
      (conde
       ((== #t e))
       ((== #f e))))

     ((fresh (lhs rhs)
             (== `(cons ,lhs ,rhs) e)
             (fresh (t1 t2)
                    (== `(pairof ,t1 ,t2) t)
                    (!- G lhs t1) 
                    (!- G rhs t2))))

     ((fresh (x)
             (== `(car ,x) e)
             (fresh (t1)
                    (== 'Nat t) 
                    (!- G x `(pairof Nat ,t1)))))

     ((fresh (x)
             (== `(cdr ,x) e)
             (fresh (t1)
                    (== 'Nat t) 
                    (!- G x `(pairof ,t1 Nat)))))

     ((fresh (v)
             (== `(car ,v) e)
             (== t 'Bool)
             (!- G v t)))

     ((fresh (v)
             (== `(not ,v) e)
             (== t 'Bool)
             (!- G v t)))

     ((fresh (n)
             (== `(zero? ,n) e)
             (== t 'Bool)
             (!- G n 'Nat)))

     ((fresh (n)
             (== `(sub1 ,n) e)
             (== t 'Nat)
             (!- G n 'Nat)))

     ((fresh (ne1 ne2) 
             (== `(+ ,ne1 ,ne2) e)
             (== 'Nat t)
             (!- G ne1 'Nat)
             (!- G ne2 'Nat)))

     ((fresh (ne1 ne2)
             (== `(* ,ne1 ,ne2) e)
             (== 'Nat t)
             (!- G ne1 'Nat)
             (!- G ne2 'Nat)))

     ((fresh (teste anse elsee)
             (== `(if ,teste ,anse ,elsee) e)
             (!- G teste 'Bool)
             (!- G anse t)
             (!- G elsee t)))

     ((symbolo e) (apply-Go G e t))
     
     ((fresh (x b)
             (== `(lambda (,x) ,b) e)
             (symbolo x)
             (fresh (tx tb)          
                    (== `(,tx -> ,tb) t)
                    (!- `((,x . ,tx) . ,G) b tb))))

     ((fresh (x b)
             (== `(fix (lambda (,x) ,b)) e)
             (symbolo x)
             (fresh (t1)          
                    (== t1 t)
                    (!- `((,x . ,t1) . ,G) b t1))))

     ((fresh (e1 arg)
             (== `(,e1 ,arg) e)
             (fresh (targ)
                    (!- G e1 `(,targ -> ,t))
                    (!- G arg targ)))
      ))))

;;; tests
(check-expect (run* (q) (!- '() #t q)) `(Bool))
(check-expect (run* (q) (!- '() 17 q)) `(Nat))
(check-expect (run* (q) (!- '() '(zero? 24) q)) `(Bool))
(check-expect (run* (q) (!- '() '(zero? (sub1 24)) q)) `(Bool))
(check-expect (run* (q) (!- '() '(not (zero? (sub1 24))) q)) `(Bool))

(check-expect (run* (q)
                   (!- '() '(zero? (sub1 (sub1 18))) q)) `(Bool))

(check-expect(run* (q)
                   (!- '()  '(lambda (n) (if (zero? n) n n)) q)) `((Nat -> Nat)))

(check-expect (run* (q)
                    (!- '() '((lambda (n) (zero? n)) 5) q)) `(Bool))

(check-expect (run* (q)
                    (!- '() '(if (zero? 24) 3 4) q)) `(Nat))

(check-expect (run* (q)
                    (!- '() '(if (zero? 24) (zero? 3) (zero? 4)) q)) `(Bool))

(check-expect(run* (q)
                   (!- '() '(lambda (x) (sub1 x)) q)) `((Nat -> Nat)))

(check-expect (run* (q)
                    (!- '() '(lambda (a) (lambda (x) (+ a x))) q)) `((Nat -> (Nat -> Nat))))

(check-expect (run* (q)
                    (!- '() '(lambda (f)
                               (lambda (x)
                                 ((f x) x))) q))
              `(((_.0 -> (_.0 -> _.1)) -> (_.0 -> _.1))))

(check-expect  (run* (q)
                     (!- '() '(sub1 (sub1 (sub1 6))) q)) `(Nat))

(check-expect (run 1 (q)
                   (fresh (t)
                          (!- '() '(lambda (f) (f f)) t))) `())

(check-expect (length (run 20 (q)
                           (fresh (lam a b)
                                  (!- '() `((,lam (,a) ,b) 5) 'Nat)
                                  (== `(,lam (,a) ,b) q))))
              20)

(check-expect (length (run 30 (q) (!- '() q 'Nat))) 30)

(check-expect (length (run 30 (q) (!- '() q '(Nat -> Nat)))) 30)

(check-expect (length (run 500 (q) (!- '() q '(Nat -> Nat)))) 500)

(check-expect (length (run 30 (q) (!- '() q '(Bool -> Nat)))) 30)

(check-expect (length (run 30 (q) (!- '() q '(Nat -> (Nat -> Nat))))) 30)

(check-expect (length (run 100 (q)
                           (fresh (e t)
                                  (!- '() e t)
                                  (== `(,e ,t) q)))) 100)

(check-expect (length (run 100 (q)
                           (fresh (g e t)
                                  (!- g e t)
                                  (== `(,g ,e ,t) q)))) 100)

(check-expect (length
               (run 100 (q)
                    (fresh (g v)
                           (!- g `(var ,v) 'Nat)
                           (== `(,g ,v) q)))) 100)

(check-expect (run 1 (q)
                   (fresh (g)
                          (!- g
                              `((fix (lambda (!)
                                       (lambda (n)
                                         (if (zero? n)
                                             1
                                             (* n (! (sub1 n)))))))
                                5)
                              q))) `(Nat))

(check-expect (run 1 (q)
                   (fresh (g)
                          (!- g
                              '((fix (lambda (!)
                                       (lambda (n)
                                         (* n (! (sub1 n))))))
                                5)
                              q))) `(Nat))

(test)