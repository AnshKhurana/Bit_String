#lang racket
;;;;;;;;;;;;;Q3;;;;;;;;;;;;;;;;
(define (n2m n m a)
  (cond [(= n m) a] 
 [else (if (= (car a) 0) (zero-add (- m n) a) (one-add (- m n) a))]
 ))
(define (zero-add k a)
  (if (> k 0) (cons 0 (zero-add (- k 1) a)) a))
(define (one-add k a)
  (if (> k 0) (cons 1 (one-add (- k 1) a)) a))