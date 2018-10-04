#lang racket
;;;;;;;;;;;;;Q8;;;;;;;;;;;;;;;;
(define (2s-complement n x)
  (u-add n (inv x) (zero-add (- n 1) '(1))))

(define (s-div n x y)
  (cond [(and (= (car x) 0) (= (car y) 0)) (u-div n x y)]
        [(and (= (car x) 1) (= (car y) 1))
         (let* ( [x1 (2s-complement n x)]
                 [y1 (2s-complement n y)]
                 [u-div-res (u-div n x1 y1) ]
                 [newquo (2s-complement n (car u-div-res))]
                 [newrem (2s-complement n (cadr u-div-res))]
                 )
           (cons newquo (list newrem)))]
        [(and (= (car x) 1) (= (car y) 0))
         (let* ( [x1 (2s-complement n x)]
                
                 [u-div-res (u-div n x1 y)]
                 [newquo (2s-complement n (car u-div-res))]
                 [newrem (2s-complement n (cadr u-div-res))]
                 )
           (cons newquo (list newrem)))]
  [else
         (let* ( 
                 [y1 (2s-complement n y)]
                 [u-div-res (u-div n x y1)]
                 [newquo (2s-complement n (car u-div-res))]
                 [newrem (cadr u-div-res)]
                 
                 )
           (cons newquo (list newrem)))]
  ))
  
(define (inv x)
  (cond [(null? x) x]
        [else (cons (if (= (car x) 0) 1 0) (inv (cdr x)))]))

;;;;;;;;;;;;;Q7;;;;;;;;;;;;;;;;

;;;;;;;helpers used for u-div;;;;;;
(define (convs x n)
  (zero-add (- n (length x)) x))
(define (trunc l)
  (cond [(null? l) l]
        [(= (car l) 1) l]
        [(= (car l) 0) (trunc (cdr l))]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (u-div n x y)
  (cond [(check x y) (cons (zero-vector n) (list x))]
        [else
  (u-div-helper n (cdr x) y (convs (list (car x)) n) '())
  ]))


(define (u-div-helper n x y rem quo)
  (cond [(null? x)
         (if (check rem y)
             (let* ( [newquo (append quo '(0))]
                    
                     )
               (cons newquo (list rem)))
                     
             (let* ( [newquo (append quo '(1))]
                     [newrem (u-sub n rem y)]
                     
                     )
               (cons newquo (list newrem)))
         )]
        [else (if (check rem y)
                  (let* ( [newquo (append quo '(0))]
                          [newx (cdr x)]
                          [newrem (convs (trunc (append rem (list(car x)))) n) ]
                          )
                    (u-div-helper n newx y newrem newquo))
                  (let* ( [newquo (append quo '(1))]
                          [newrem (convs (trunc (append (u-sub n rem y) (list(car x))))  n) ]
                          [newx (cdr x)]
                          )
                    (u-div-helper n newx y newrem newquo)))]
        ))
;;;;;;;helpersfromQ3;;;;;;;;;;;;;;;;
(define (n2m n m a)
  (cond [(= n m) a] 
 [else (if (= (car a) 0) (zero-add (- m n) a) (one-add (- m n) a))]
 ))
(define (zero-add k a)
  (if (> k 0) (cons 0 (zero-add (- k 1) a)) a))
(define (one-add k a)
  (if (> k 0) (cons 1 (one-add (- k 1) a)) a))

;;;;;;;helpersfromQ4;;;;;;;;;;;;;;;;

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

