#lang racket
;;;;;;;;;;;;;Q9;;;;;;;;;;;;;;;;
(define (fix2d n a)

  (+ (b2u n (slice a 1 n)) (fix2d-h (slice a (+ n 1) (* 2 n))))
  )
  

(define (fix2d-h a)
  (cond [(null? a) 0]
        [(+ (/ (car a) 2) (/ (fix2d-h  (cdr a)) 2)) ] 
  ))
 (define (d2fix n a)
   (append (u2b  n (inexact->exact (truncate a))) (f2b n (frac a) 1))
   )
(define (frac a)
  (- a (truncate a)))
   

(define (f2b n x ctr)
  (cond [ (= ctr n) (cons (inexact->exact (truncate (* 2 x))) '()) ]
  [else (cons (inexact->exact (truncate (* 2 x))) (f2b n (frac (* 2 x)) (+ ctr 1))
)]
  ))
(define (slice l i k)
   (slice-h l '() 1 i k)
    )
 (define (slice-h l r c i k)
   (cond [(null? l) (reverse r)]
         [(or (< c i) (> c k)) (slice-h (cdr l) r (+ c 1) i k)]
         [else (slice-h (cdr l) (cons (car l) r) (+ c 1) i k)]
         ))
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