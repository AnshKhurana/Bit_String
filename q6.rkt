#lang racket
(define (s-mult n x y)
  (let* ([x1 (n2m  n (* 2 n) x)]
         [y1 (n2m  n (* 2 n) y)]
         [res (u-mult (* 2 n) x1 y1)]
         )
 (slice  res (+ (* 2 n) 1) (* 4 n))))
(define (u-mult n x y)
 
  (u-mult-helper n x y (- n 1) (zero-vector (* 2 n))
  ))

(define (u-mult-helper n x y ctr res)
(define (single-bit-mult x y)
  (if (= y 1) x (zero-vector n)))
  

  
  (cond [(null? (cdr y)) (u-add (* 2 n) res (zero-add (- (* 2 n) n) (single-bit-mult x (car y))))]
        [else (u-mult-helper n x (cdr y) (- ctr 1) (u-add (* 2 n) res (zero-add (- (* 2 n) (+ n ctr)) (shift-l (single-bit-mult x (car y)) ctr ))))]
        )

  )
(define (shift-l x m)
  (append x (zero-vector m))
     )
(define (zero-vector n)
  
      (make-list n 0)
        )

(define (u-add n a b)
 (car (u-add-h n a b)))
(define (u-add-h n a b)
   (cond [(null? (cdr a)) (cons (cons (remainder (+ (car a) (car b)) 2) '()) (if (>
                                                                                  (+ (car a) (car b)) 1) 1 0))]  
         [(let* ([prev (u-add-h n (cdr a) (cdr b))]
                 [carry (cdr prev)]
                 [res (car prev)]
                 [single-add (+ (car a) (car b) carry)]
                 [newd (modulo single-add 2)]
                 [new-car (if (> single-add 1) 1 0)]
                 
                 )
                 
          (cons (cons newd res) new-car))
            ]
))
;;;;;;;;;;;;;Q3;;;;;;;;;;;;;;;;
(define (n2m n m a)
  (cond [(= n m) a] 
 [else (if (= (car a) 0) (zero-add (- m n) a) (one-add (- m n) a))]
 ))
(define (zero-add k a)
  (if (> k 0) (cons 0 (zero-add (- k 1) a)) a))
(define (one-add k a)
  (if (> k 0) (cons 1 (one-add (- k 1) a)) a))
(define (slice l i k)
   (slice-h l '() 1 i k)
    )
 (define (slice-h l r c i k)
   (cond [(null? l) (reverse r)]
         [(or (< c i) (> c k)) (slice-h (cdr l) r (+ c 1) i k)]
         [else (slice-h (cdr l) (cons (car l) r) (+ c 1) i k)]
         ))



