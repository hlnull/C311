#lang racket
(require "pmatch.rkt")
;1
(define list-ref
  (lambda (ls n)
    (letrec
      ((nth-cdr
         (lambda (n)
           (if (eq? n 0) ls (cdr (nth-cdr (sub1 n)))))))
      (car (nth-cdr n)))))
;2
(define (union set1 set2)
  (cond
    ((eq? set1 null) set2)
     ((eq? set2 null) set1)
     (else (if (not (memv (car set1) set2))
                       (cons (car set1) (union (cdr set1) (remv (car set1) set2)))
                       (union (cdr set1) set2)))))
;3
(define (extend x pred)
        (lambda (y)
          (cond
             ((eqv? x y) #t)
             ((pred y) #t)
             (else #f))))
;4
(define (walk-symbol x ls)
    (let ([s (assv x ls)])
      (if (not s) x (walk-symbol (cdr s) ls))))
;5
(define (lambda->lumbda exp)
  (pmatch exp
          [`,x (guard (not (pair? x))) x]
          [`(lambda (,x) ,body)
           `(lumbda (,x) ,(lambda->lumbda body))]
          [`(,rator,rand)
           `(,(lambda->lumbda rator)
             ,(lambda->lumbda rand))]
          ))
;6
(define (var-occurs? var exp)
  (pmatch exp
      [`,x (guard (not (pair? x))) (eq? var x)]
      [`(lambda (,x) ,body)
       (var-occurs? var body)]
      [`(,rator,rand)
       (or (var-occurs? var rator) (var-occurs? var rand))]))
;7
(define (vars exp)
  (pmatch exp
          [`,x (guard (not (pair? x)))
               `(,x)]
          [`(lambda (,x) ,body)
           (vars body)]
          [`(,rator,rand)
           (append (vars rator) (vars rand))]))
;8
(define (unique-vars exp)
    (pmatch exp
          [`,x (guard (not (pair? x)))
               `(,x)]
          [`(lambda (,x) ,body)
           (unique-vars body)]
          [`(,rator,rand)
           (union (unique-vars rator) (unique-vars rand))]))
;9
(define (var-occurs-free? var exp)
    (pmatch exp
          [`,x (guard (not (pair? x)))
               (eq? x var)]
          [`(lambda (,x) ,body)
           (if (eq? x var)
               #f
               (var-occurs-free? var body))]
          [`(,rator ,rand)
           (or (var-occurs-free? var rator)
               (var-occurs-free? var rand))]))
;10
(define (var-occurs-bound? var exp)
      (pmatch exp
          [`,x (guard (symbol? x)) #f]
          [`(lambda (,x) ,body)
           (or (var-occurs-bound? var body) (if (eq? var x) (var-occurs-free? var body) #f))]
          [`(,rator ,rand)
           (or (var-occurs-bound? var rator)
               (var-occurs-bound? var rand))]))
;11
(define (unique-free-vars exp)
  (pmatch exp
          [`,x (guard (symbol? x)) `(,x)]
          [`(lambda (,x) ,body)
           (remv x (unique-free-vars body))]
          [`(,rator,rand)
           (union (unique-free-vars rator) (unique-free-vars rand))]))
;12
(define (unique-bound-vars exp)
  (pmatch exp
          [`,x (guard (symbol? x)) `()]
          [`(lambda (,x) ,body)
           (if (memv x (unique-vars body))
               (cons x (unique-bound-vars body))
               (unique-bound-vars body))]
          [`(,rator,rand)
           (union (unique-bound-vars rator) (unique-bound-vars rand))]))
;13
(define (lex exp acc)
  (pmatch exp
          [`,x (guard (symbol? x))
               (if (memv x acc)
                   `(var ,(- (length acc) (length (memv x acc))))
                   `())]
          [`(lambda (,x) ,body)
           `(lambda ,(lex body (cons x acc)))]
          [`(,rator,rand)
           `(,(lex rator acc)
             ,(lex rand acc))]))
;14
(define a-list `((c . ,(box 15)) (e . ,(box 'f)) (b . ,(box 'c)) (a . ,(box 'b))))
(define b-list `((c . ,(box 15)) (e . ,(box 'f)) (b . ,(box 'c)) (a . ,(box 'b)) (d . ,(box '1))))

;(set-box! (cdr (car a-list)) 1)
;a-list
(define (walk-symbol-update x ls)
  (letrec ((helper
            (lambda (x ls acc)
              (let ([s (assv x ls)])
                (if (not s) (foldr (lambda (a b)
                                     (if (eq? (unbox (cdr a)) x) x
                                         (set-box! (cdr a) x))) x acc)
                    (helper (unbox (cdr s)) (remv s ls) (cons s acc)))))))
    (helper x ls '())))