; Identity Monad
(define return-id
  (lambda (a) a))


(define bind-id
  (lambda (ma f)
    (f ma)))


;
(define plus-id
  (lambda (a b)
    (bind-id
     (return-id (+ a b))
     (lambda (x) (return-id x)))))


(plus-id 1 2)


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


;
(define divide-maybe
  (lambda (a b)
    (if (zero? b)
        (fail)
        (return-maybe (/ a b)))))


(define-syntax do
  (syntax-rules (<-)
    ((_ bind e) e)
    ((_ bind (v <- e) e* e** ...)
     (bind e (lambda (v) (do bind e* e** ...))))
    ((_ bind e e* e** ...)
     (bind e (lambda (_) (do bind e* e** ...))))))


(bind-maybe
 (return-maybe (+ 7 8))
 (lambda (x)
   (bind-maybe
    (divide-maybe x 4)
    (lambda (x^)
      (return-maybe x^)))))


(do bind-maybe
    (x <- (return-maybe (+ 7 8)))
    (x^ <- (divide-maybe x 4))
    (return-maybe x^))



;test :: Maybe Double 
;test = do 
;	x <- Just (7 + 8)
;	y <- Just (x / 4)
;	return y


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


;
(define reciprocals
  (lambda (l)
    (cond
      [(null? l) (return-writer `())]
      [(zero? (car l))
       (bind-writer
        (tell-writer "Saw a 0")
        (lambda (_)
          (reciprocals (cdr l))))]
      [else
       (bind-writer
        (reciprocals (cdr l))
        (lambda (d)
          (return-writer
           (cons (/ 1 (car l)) d))))])))


(define reciprocals^
  (lambda (l)
    (cond
      [(null? l) `()]
      [(zero? (car l)) (append (reciprocals^ (cdr l)) `("Saw a 0"))]
      [else (cons (/ 1 (car l)) (reciprocals^ (cdr l)))])))


(define reciprocals^^
  (lambda (l)
    (cond
      [(null? l) (return-writer `())]
      [(zero? (car l)) (do bind-writer
                           (tell-writer "Saw a 0")
                           (reciprocals^^ (cdr l)))]
      [else
       (do bind-writer
         (d <- (reciprocals^^ (cdr l)))
         (return-writer (cons (/ 1 (car l)) d)))])))


(reciprocals^^ `(0 1 2 3 4 5 6 7))
(reciprocals^ `(0 1 2 3 4 5 6 7))
(reciprocals `(0 1 2 3 4 5 6 7))


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


;(define even-length?
;  (lambda (l s)
;    (cond
;      [(null? l) s]
;      [else (even-length? (cdr l) (not s))])))


(define even-length?
  (lambda (l)
    (cond
      [(null? l) (return-state `_)]
      [else
       (do bind-state
         (s <- get-state)
         (put-state (not s))
         (even-length? (cdr l)))])))


((even-length? `(1 2 3 4)) #t)


(define countevens
  (lambda (l)
    (cond
      [(null? l) 0]
      [(pair? (car l)) (+ (countevens (car l)) (countevens (cdr l)))]
      [(or (null? (car l)) (odd? (car l))) (countevens (cdr l))]
      [else (add1 (countevens (cdr l)))])))


(define remberevens
  (lambda (l)
    (cond
      [(null? l) `()]
      [(pair? (car l)) (cons (remberevens (car l)) (remberevens (cdr l)))]
      [(or (null? (car l)) (odd? (car l))) (cons (car l) (remberevens (cdr l)))]
      [else (remberevens (cdr l))])))


(define remberevensXcountevens^
  (lambda (l) `(,(remberevens l) . ,(countevens l))))


(remberevensXcountevens^ `(1 2 3 4 5 6 7 8 9 10))


(define remberevensXcountevens
  (lambda (l)
    (cond
      [(null? l) (return-state `())]
      [(pair? (car l))
       (do bind-state
         (a <- (remberevensXcountevens (car l)))
         (d <- (remberevensXcountevens (cdr l)))
         (return-state (cons a d)))]
      [(or (null? (car l)) (odd? (car l)))
       (do bind-state
         (d <- (remberevensXcountevens (cdr l)))
         (return-state (cons (car l) d)))]
      [else
       (do bind-state
         (s <- get-state)
         (put-state (add1 s))
         (remberevensXcountevens (cdr l)))])))


; we must somehow seed the state, and so we start it with a 0
((remberevensXcountevens `(1 2 3 4 5 6 7 8 9 10)) 0)
