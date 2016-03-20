#lang racket
(define empty-k
  (lambda ()
    (let ((once-only #f))
      (lambda (v)
        (if once-only
	    (error 'empty-k "You can only invoke the empty continuation once")
	    (begin (set! once-only #t) v))))))
;1
(define binary-to-decimal
  (lambda (n)
    (cond
      [(null? n) 0]
      [else (+ (car n) (* 2 (binary-to-decimal (cdr n))))])))

(define binary-to-decimal-cps
  (lambda (n k)
    (cond
      [(null? n) (k 0)]
      [else (binary-to-decimal-cps (cdr n) (lambda (x) (k (+ (car n) (* 2 x)))))])))

(binary-to-decimal-cps '() (empty-k))
(binary-to-decimal-cps '(1) (empty-k))
(binary-to-decimal-cps '(0 1) (empty-k))
(binary-to-decimal-cps '(1 1 0 1) (empty-k))

;2
(define times
  (lambda (ls)
    (cond
      [(null? ls) 1]
      [(zero? (car ls)) 0]
      [else (* (car ls) (times (cdr ls)))])))

(define times-cps
  (lambda (ls k)
    (cond
      [(null? ls) (k 1)]
      [(zero? (car ls)) (k 0)]
      [else (times-cps (cdr ls) (lambda (x) (k (* (car ls) x))))])))

(times-cps '(1 2 3 4 5) (empty-k))
(times-cps '(1 2 3 0 3) (empty-k))

;4
(define plus
  (lambda (m)
    (lambda (n)
      (+ m n))))

(define plus-cps
  (lambda (m)
    (lambda (n k)
      (k (+ m n)))))

((plus-cps 2) 3 (empty-k))
((plus-cps ((plus-cps 2) 3 (empty-k))) 5 (empty-k))

;5
(define remv-first-9*
  (lambda (ls)
    (cond
      [(null? ls) '()]
      [(pair? (car ls))
       (cond
         [(equal? (car ls) (remv-first-9* (car ls)))
          (cons (car ls) (remv-first-9* (cdr ls)))]
         [else (cons (remv-first-9* (car ls)) (cdr ls))])]
      [(eqv? (car ls) '9) (cdr ls)]
      [else (cons (car ls) (remv-first-9* (cdr ls)))])))

(define remv-first-9*-cps
  (lambda (ls k)
    (cond
      [(null? ls) (k '())]
      [(pair? (car ls))
       (remv-first-9*-cps (car ls) (lambda (x^)
                                     (cond
                                        [(equal? (car ls) x^)
                                         (remv-first-9*-cps (cdr ls) (lambda (x^^)
                                                                       (k (cons (car ls) x^^))))]
                                        [else (remv-first-9*-cps (car ls) (lambda (x^^^)
                                                                            (k (cons x^^^ (cdr ls)))))])))]
      [(eqv? (car ls) '9) (k (cdr ls))]
      [else (remv-first-9*-cps (cdr ls) (lambda (x) (k (cons (car ls) x))))])))

(remv-first-9*-cps '((1 2 (3) 9)) (empty-k))
(remv-first-9*-cps '(9 (9 (9 (9)))) (empty-k))
(remv-first-9*-cps '(((((9) 9) 9) 9) 9) (empty-k))

;6
(define cons-cell-count
  (lambda (ls)
    (cond
      [(pair? ls)
       (add1 (+ (cons-cell-count (car ls)) (cons-cell-count (cdr ls))))]
      [else 0])))

(define cons-cell-count-cps
  (lambda (ls k)
    (cond
      [(pair? ls)
       (cons-cell-count-cps (car ls) (lambda (x^)
                                       (k (add1 (cons-cell-count-cps (cdr ls) (lambda (x^^)
                                                                             (+ x^ x^^)))))))]
      [else (k 0)])))

(cons-cell-count-cps '(((((0)))) 1 2 3 4 5) (empty-k))
(cons-cell-count-cps '(((((0))))) (empty-k))
(cons-cell-count-cps (cons (cons 2 3) (cons (cons 5 6) (cons '() '()))) (empty-k))

;7
(define find 
  (lambda (u s)
    (let ((pr (assv u s)))
      (if pr (find (cdr pr) s) u))))

(define find-cps 
  (lambda (u s k)
    (let ((pr (assv u s)))
      (if pr (find-cps (cdr pr) s k) u))))

(find-cps 5 '((5 . a) (6 . b) (7 . c)) (empty-k))
(find-cps 7 '((5 . a) (6 . 5) (7 . 6)) (empty-k))
(find 5 '((5 . 6) (9 . 6) (2 . 9)))

;8
(define ack
  (lambda (m n)
    (cond
      [(zero? m) (add1 n)]
      [(zero? n) (ack (sub1 m) 1)]
      [else (ack (sub1 m)
                 (ack m (sub1 n)))])))

(define ack-cps
  (lambda (m n k)
    (cond
      [(zero? m) (k (add1 n))]
      [(zero? n) (ack-cps (sub1 m) 1 k)]
      [else (ack-cps m (sub1 n) (lambda (x)
                                  (ack-cps (sub1 m) x k)))])))

(ack-cps 3 2 (empty-k))
(ack-cps 1 2 (empty-k))

;9
(define fib
  (lambda (n)
    ((lambda (fib)
       (fib fib n))
     (lambda (fib n)
       (cond
	 [(zero? n) 0]
	 [(= 1 n) 1]
	 [else (+ (fib fib (sub1 n)) (fib fib (sub1 (sub1 n))))])))))

(define fib-cps
  (lambda (n k)
    ((lambda (fib-cps k^)
       (fib-cps fib-cps n k^))
     (lambda (fib-cps n k^^)
       (cond
	 [(zero? n) (k^^ 0)]
	 [(= 1 n) (k^^ 1)]
	 [else (fib-cps fib-cps (sub1 n) (lambda (x^)
                                           (k^^ (fib-cps fib-cps (sub1 (sub1 n)) (lambda (x^^)
                                                                                   (+ x^ x^^))))))])) k)))

(fib-cps 6 (empty-k))
(fib-cps 21 (empty-k))

;10
(define unfold
  (lambda (p f g seed)
    ((lambda (h)
       ((h h) seed '()))
     (lambda (h)
       (lambda (seed ans)
	 (if (p seed)
	     ans
	     ((h h) (g seed) (cons (f seed) ans))))))))

(unfold null? car cdr '(a b c d e))

(define null?-cps
    (lambda (ls k)
      (k (null? ls))))

(define car-cps
    (lambda (pr k)
      (k (car pr))))

(define cdr-cps
    (lambda (pr k)
      (k (cdr pr))))

(define unfold-cps
  (lambda (p f g seed k)
    ((lambda (h k^)
       (h h (lambda (x^) (x^ seed '() k^))))
     (lambda (h k^^)
       (k^^ (lambda (seed ans k^^^)
	 (p seed (lambda (x^^) (if x^^
	     (k^^^ ans)
	     (h h (lambda (x^^^)
                    (g seed (lambda (x^^^^)
                              (f seed (lambda (x^^^^^)
                                        (x^^^ x^^^^ (cons x^^^^^ ans) k^^^))))))))))))) k)))

(unfold-cps null?-cps car-cps cdr-cps '(a b c d e) (empty-k))

