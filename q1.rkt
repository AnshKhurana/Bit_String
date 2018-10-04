#lang racket
;;;;;;;;;;;;;Q1;;;;;;;;;;;;;;;;
(define (b2u n a)
  (b2u-h n a 0 1))

(define (b2u-h n l x ctr)
  (cond [(or (> ctr n) (null? l)) x]
        [else (b2u-h n (cdr l) (+ (* 2 x)  (car l)) (+ ctr 1))]
        ))
(define (u2b n x)
  (cond [( or (>= x (expt 2 n) ) (< x 0)) (displayln "Out of range")]
        [else (u2b-h n '() x 1)]
        ))
(define (u2b-h n l x c) 
  (cond [(and (= x 0) (> c n)) l]
        [(= x 0) (u2b-h n (cons 0 l) 0 (+ c 1))] 
        [else (u2b-h n (cons (modulo x 2) l) (quotient x 2) (+ c 1))]
        ))

(define l 23)  ;; significand length
(define k 8)  ;; exponent length 
(define n 16)  ;; length of signed/unsigned integers

(u2b n 14)
(u2b n 23)
(u2b n 123)
(u2b n 256)
