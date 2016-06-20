#lang racket
(require "mk.rkt")
(require "numbers.rkt")

;; Part I Write the answers to the following problems using your
;; knowledge of miniKanren.  For each problem, explain how miniKanren
;; arrived at the answer.  You will be graded on the quality of your
;; explanation; a full explanation will require several sentences.

;; 1 What is the value of 

(run 2 (q)
  (== 5 q)
  (conde
   [(conde 
     [(== 5 q)
      (== 6 q)])
    (== 5 q)]
   [(== q 5)]))

;; 2 What is the value of
(run 1 (q) 
  (fresh (a b) 
    (== `(,a ,b) q)
    (absento 'tag q)
    (symbolo a)))

;; 3 What do the following miniKanren constraints mean?
;; a ==
;; b =/=
;; c absento
;; d numbero
;; e symbolo

;; Part II goes here.
;;
(define assoc
  (lambda (x ls)
    (match-let* ((`(,a . ,d) ls)
                 (`(,aa . ,da) a))
      (cond
        ((equal? aa x) a)
        ((not (equal? aa x)) (assoc x d))))))

(define assoco
  (lambda (x ls out)
    (fresh (a d aa da)
           (== `(,a . ,d) ls)
           (== `(,aa . ,da) a)
           (conde
            ((== aa x) (== a out))
            ((=/= aa x) (assoco x d out))))))

;;
(define reverse
  (lambda (ls)
    (cond
      ((equal? `() ls) `())
      (else
       (match-let* ((`(,a . ,d) ls)
                    (res (reverse d)))
         (append res `(,a)))))))

(define reverseo
  (lambda (ls out)
    (conde
     ((== `() ls) (== `() out))
     ((fresh (a d res)
             (== `(,a . ,d) ls)
             (reverseo d res)
             (appendo res `(,a) out))))))

;;
(define stutter
  (lambda (ls)
    (cond
      ((equal? `() ls) `())
      (else 
        (match-let* ((`(,a . ,d) ls)
		     (res (stutter d)))
          `(,a ,a . ,res))))))

(define stuttero
  (lambda (ls out)
    (conde
      ((== '() ls) (== '() out))
      ((fresh (a d res) 
              (== `(,a . ,d) ls)
              (== out `(,a ,a . ,res))
              (stuttero d res))))))