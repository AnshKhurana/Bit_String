#lang racket
;;;;;;;;;;;;;Q4;;;;;;;;;;;;;;;;

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
(define (u-sub n a b)
  (cond [ (check a b ) (zero-vector n)]
 [else (car (u-sub-h n a b)) ]))

(define (u-sub-h n a b)
   (cond [(null? (cdr a)) (cons (cons (if (< (car a) (car b)) 1 (- (car a) (car b))) '()) (if (<
                                                                                  (car a) (car b))  1 0))]  
         [(let* ([prev (u-sub-h n (cdr a) (cdr b))]
                 [carry (cdr prev)]
                 [res (car prev)]
                 [single-sub (- (car a) (car b) carry)]
                 [newd (modulo single-sub 2)]
                 [new-car (if (< single-sub 0) 1 0)]
                 
                 )
                 
          (cons (cons newd res) new-car))
            ]
))
(define (check a b)
 
  (cond [(null? a) #t]
        [(equal? a b) #f]
        [ (= (car a) 0) (if (= (car b) 0) (check (cdr a) (cdr b) ) #t)]
        [else (if (= (car b) 1) (check (cdr a) (cdr b)) #f)]))
(define (zero-vector n)
  
      (make-list n 0)
        )
