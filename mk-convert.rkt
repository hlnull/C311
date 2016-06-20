#lang racket
(require "mk.rkt")

(define (list-union s1 s2)
  (cond
    ((equal? '() s1) s2)
    ((pair? s1)
     (let ((a (car s1))
           (d (cdr s1)))
       (cond
         ((member a s2) (list-union d s2))
         (else (cons a (list-union d s2))))))))

(list-union '() '(1 2 3))
(list-union '(2) '(3 2 1))
(list-union '(1 2) '(4 5))

(define-syntax let-pair
  (syntax-rules ()
    [(_ ((a . d) call) body)
     (let ((tmp-res call))
       (let ((a (car tmp-res))
             (d (cdr tmp-res)))
         body))]))

(define (list-union^ s1 s2)
  (cond
    ((equal? '() s1) s2)
    ((pair? s1)
     (let-pair ((a . d) s1)
               (cond
                 ((member a s2) (list-union d s2))
                 (else (cons a (list-union d s2))))))))

(list-union^ '() '(1 2 3))
(list-union^ '(2) '(3 2 1))
(list-union^ '(1 2) '(4 5))

(define (member?o x ls out)
  (conde
   ((== '() ls) (== #f out))
   ((fresh (a d)
           (== `(,a . ,d) ls)
           (conde
            ((== x a) (== #t out))
            ((=/= x a) (member?o x d out)))))))

(define (list-uniono s1 s2 out)
  (conde
   ((== '() s1) (== s2 out))
   ((fresh (a d)
           (== `(,a . ,d) s1)
           (fresh (b res)
                  (conde
                   ((== b #t) (== res out))
                   ((== b #f) (== `(,a . ,res) out)))
                  (member?o a s2 b)
                  (list-uniono d s2 res))))))

(run 1 (q)
     (list-uniono '(1 2 3 4) '(5 6) q))

(run 5 (q)
     (fresh (a b) (== q `(,a ,b))
            (list-uniono a b '(1 2))))