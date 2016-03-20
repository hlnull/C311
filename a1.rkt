#lang racket
;1
(define countdown
  (lambda (n)
    (cond
      ((zero? n) '(0))
      (else (cons n (countdown (sub1 n)))))))
;2
(define insertR
  (lambda (x y ls)
    (cond
      ((null? ls) '())
      ((eq? x (car ls)) (append (list x y) (insertR x y (cdr ls))))
      (else (cons (car ls) (insertR x y (cdr ls)))))))
;3
(define remv-1st
  (lambda (x ls)
    (cond
      ((null? ls) '())
      ((eq? x (car ls)) (cdr ls))
      (else (cons (car ls) (remv-1st x (cdr ls)))))))
;4
(define list-index-ofv?
  (lambda (x ls)
    (cond
      ((null? ls) (error "bad_data"))
      ((eq? x (car ls)) 0)
      (else (+ 1 (list-index-ofv? x (cdr ls)))))))
;5
(define filter
  (lambda (predicate ls)
    (cond
      ((null? ls) '())
      ((eq? (predicate (car ls)) #t)  (cons (car ls) (filter predicate (cdr ls))))
      (else (filter predicate (cdr ls))))))
;6
(define zip
  (lambda (l1 l2)
    (cond
      ((null? l1) '())
      ((null? l2) '())
      (else (cons (cons (car l1) (car l2)) (zip (cdr l1) (cdr l2)))))))
;7
(define map
  (lambda (p ls)
    (cond
      ((null? ls) '())
      (else (cons (p (car ls)) (map p (cdr ls)))))))
;8
(define append
  (lambda (ls1 ls2)
    (cond
      ((null? ls1) ls2)
      (else (cons (car ls1) (append (cdr ls1) ls2))))))
;9
(define reverse
  (lambda (ls)
    (cond
      ((null? ls) '())
      (else (append (reverse (cdr ls)) (list (car ls)))))))
;10
(define fact
  (lambda (x)
    (cond
      ((zero? x) 1)
      (else (* x (fact (sub1 x)))))))
;11
(define memv
  (lambda (x ls)
    (cond
      ((null? ls) #f)
      ((eq? (car ls) x) ls)
      (else (memv x (cdr ls))))))
;12
(define fib
  (lambda (x)
    (cond
      ((zero? x) 0)
      ((<= x 2) 1)
      (else (+ (fib (sub1 x)) (fib (- x 2)))))))
;13
; ((w x) y (z))
;14
(define binary->natural
  (lambda (ls)
    (cond
      ((null? ls) 0)
      (else (+ (car ls) (* 2 (binary->natural (cdr ls))))))))
;15
(define minus
  (lambda (n m)
    (cond
      ((zero? n) 0)
      ((zero? m) n)
      (else (minus (sub1 n) (sub1 m))))))
;16
(define div
  (lambda (n m)
    (cond
      ((negative? n) error "bad data")
      ((zero? n) 0)
      (else (+ 1 (div (- n m) m))))))
;17
(define append-map
  (lambda (p ls)
    (cond
      ((null? ls) '())
      (else (append (p (car ls)) (append-map p (cdr ls)))))))
;18
(define set-difference
  (lambda (set1 set2)
    (cond
      ((null? set1) '())
      ((null? set2) set1)
      (else (set-difference (filter (lambda (x) (not (eq? x (car set2)))) set1) (cdr set2))))))
;19
(define powerset
  (lambda (set)
    (cond
      ((null? set) '(()))
      (else (append (powerset (cdr set)) (map (lambda (subset)
                                             (cons (car set) subset)) (powerset (cdr set))))))))
;20
; cartesian-product
;21
; (insertR-fr 'x 'y '(x z z x y x))
(define (insertR-fr x y ls)
  (foldr (lambda (a b)
           (if (eq? a x) (append (list a y) b) (cons a b))) '() ls))
; (filter-fr even? '(1 2 3 4 5 6))
(define (filter-fr pred seq)
  (foldr (lambda (x y)
         (if (pred x) (cons x y) y)) '() seq))
; (map-fr add1 '(1 2 3 4))
(define (map-fr func ls)
  (foldr (lambda (x y) (cons (func x) y)) '() ls))
; (append-fr '(a b c) '(1 2 3))
(define (append-fr ls1 ls2)
  (foldr cons ls2 ls1))
; (reverse-fr '(a 3 x))
(define (reverse-fr ls)
  (foldr (lambda (x y) (append y (list x))) '() ls))
; (binary->natural '(0 0 1 1))
(define (binary->natural-fr ls)
  (foldr (lambda (x y)
           (+ x (* 2 y))) 0 ls))
; (append-map-fr countdown (countdown 5))
(define (append-map-fr proc ls)
  (foldr (lambda (x y)
           (append (proc x) y)) '() ls))
; (set-difference-fr '(1 2 3 4 5) '(2 4 6 8))
(define (set-difference-fr set1 set2)
  (foldr (lambda (x y)
           (append (if (not (member x set2)) (list x) '()) y)) '() set1)) 
; (powerset-fr '(x y z))
(define (powerset-fr ls)
  (foldr (lambda (x y)
           (append y (map (lambda (subset)
                            (cons x subset)) y))) '(()) ls))
; (cartesian-product-two '(1 2) '(3 4))
(define (cartesian-product-two set1 set2)
  (map (lambda (x)
        (map (lambda (y)
               (cons x y))
             set2))
  set1))

(define (flatmap proc seq)
  (foldr append '() (map proc seq)))

; (cartesian-product-fr '((1 2) (3 4)))
(define (cartesian-product-fr sets)
  (foldr (lambda (set1 set2)
           (flatmap (lambda (x)
                      (map (lambda (y)
                             (cons x y))
                           set2))
                    set1))
         '(())
         sets))
; 22