#lang racket
(define (u2b n x)
  (cond [( or (>= x (expt 2 n) ) (< x 0)) (displayln "Out of range")]
        [else (u2b-h n '() x 1)]
        ))
(define (u2b-h n l x c) 
  (cond [(and (= x 0) (> c n)) l]
        [(= x 0) (u2b-h n (cons 0 l) 0 (+ c 1))] 
        [else (u2b-h n (cons (modulo x 2) l) (quotient x 2) (+ c 1))]
        ))
;;;;;;;;;;;;;Q2;;;;;;;;;;;;;;;;
(define (b2s n a)
  (b2s-h n a 0 1))

(define (b2s-h n l x ctr)
  (cond [(or (null? l) (> ctr n)) x]
        [(= ctr 1) (b2s-h n (cdr l) (+ (* 2 x)  (* -1 (car l))) (+ ctr 1))]
        [else (b2s-h n (cdr l) (+ (* 2 x)  (car l)) (+ ctr 1))]
        ))
(define (s2b n x)
  (cond [( or (> x (- (expt 2 (- n 1)) 1) ) (< x (* -1 (expt 2 (- n 1))))) (display "Out of range")]
        [else (u2b-h n '() (modulo x (expt 2 n)) 1)]
        ))