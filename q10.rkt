#lang racket

;;;;;;;;;;;;;Q10;;;;;;;;;;;;;;;
(define (float2d k l a)
  (define bias (- (expt 2 (- k 1)) 1))
  (define sign (if (= (car a) 1) -1 1))
  (define e (cadr a))
  (define f (caddr a))
  (cond [(equal? e (zero-vector k)) (if (equal? f (zero-vector l)) (* sign 0.0)
                                               (fix2d-h (zero-add (- bias 1) f )))]
        [(equal? e (make-list k 1)) (if (equal? f ( zero-vector l))
                                               (if (= sign 1) (displayln "+inf") (displayln "-inf"))
                                              (displayln "NaN"))]
        [else (float2d-helper (- (b2u k e) bias) sign l f)]
        ))
(define (float2d-helper shiftby s l f)
  (let* ([bitstring (shifter (cons 1 f) shiftby)]
         [x (if (> shiftby 0) shiftby (- shiftby))]
         )
    (cond [(>= shiftby 0)
           (* s (+ (b2u (+ 1 x) (slice bitstring 1  (+ 1 x)))
                   (fix2d-h (slice bitstring (+ 2 x) (+ 1 l x)))))
           ]
          [else   (* s (fix2d-h (slice bitstring 2 (+ 1 l x))))] 

    )))
       
    
(define (shifter str x)
  (cond [(> x 0) (shift-l str x)]
        [else (zero-add (- x) str)]
))
;;;;;;;;;;d2float;;;;;;;;;;;;;;
(define (d2float k l a)
  (let* ( [x (if (>= a 0) a (- a))]
          [sb (if (>= a 0) 0 1)]
          [bias (- (expt 2 (- k 1)) 1)]
          [max (- (expt 2 (+ bias 1)) (expt 2 (- bias l)))]
          [min (expt 2 (- 1 l bias))]
          )
    (cond [(>= x max) ( append (cons sb (list (append (make-list (- k 1) 1) ( list 0)))) (list (make-list l 1)))]
          [(< x min) (append  (cons sb (list (make-list k 0))) (list (make-list l 0)))]
          [else (cons sb (d2float-helper k l bias x))]
          )))
(define (d2float-helper k l bias x)
  (let* ( [intlength (+ 1 bias )]
          [fraclength (- (+ l bias ) 1)]
          [int-part (inexact->exact (truncate x))]
          [frac-part (- x (truncate x))]
          [binary-int (u2b intlength int-part)]
          [binary-frac (f2b fraclength frac-part 1)]
          [pos (pos-one binary-int)]
          [str-l (- (length  binary-int) pos)]
          [min (- 1 bias)]
          [max bias]
          )
    (cond [(equal? int-part 0) (convert (most-significant<1 binary-frac l min) bias
                                        (getexp (append binary-int binary-frac) intlength fraclength  min max ) k)]
          [else (convert (most-significant>1 binary-int binary-frac  l)
                          bias  (getexp (append binary-int binary-frac) intlength fraclength  min max ) k)]
)))

(define (most-significant<1 binary-frac l min)
 
  (cond [(underflow? binary-frac l min) (most-significant-underflow binary-frac min l )]
        
        [else (slice binary-frac (+ (pos-one binary-frac) 1) (+ (pos-one binary-frac)  l) )]
        ))
(define (underflow? binary-frac l min)
  (if (> (pos-one binary-frac) (- min)) #t #f)) 

 (define (pos-one bitstring)
   (pos-one-h bitstring 0)
   )
(define  (convert lstring bias exp k)
  (cons (u2b k (+ exp bias)) (list lstring)))
(define (pos-one-h bitstring ctr)
  (cond [(null? bitstring) ctr]
        [ (= (car bitstring) 1) (+ ctr 1)]
        [else (pos-one-h (cdr bitstring) (+ ctr 1))]
        ))
(define (most-significant-underflow binary-frac min l)
  (slice binary-frac (+ (- min ) 1) (+ (- min ) l)))

          
(define (most-significant>1 binary-int binary-frac l)
  (let* ( [ pos-1 (pos-one binary-int)]
          [str-l (- (length  binary-int) pos-1)]
)
          (if (>= str-l  l)
               (slice binary-int (+ pos-1 1) (+ pos-1 l))
                         (append (slice binary-int (+ pos-1 1) (length binary-int))
                                 (slice binary-frac 1 (- l  str-l))))
    ))
(define (getexp bitstring int-length frac-length min max)
  (let* ( [ pos-1 (pos-one bitstring)]
          [unranged-exp (- int-length pos-1)]
          
          )
    
    (cond [(>= unranged-exp max) (if(> unranged-exp max) max unranged-exp)]
          [else (if (< unranged-exp min) (- min 1) unranged-exp)]
          )))
(define (zero-vector n)
  
      (make-list n 0)
        )

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
(define (zero-add k a)
  (if (> k 0) (cons 0 (zero-add (- k 1) a)) a))

(define (one-add k a)
  (if (> k 0) (cons 1 (one-add (- k 1) a)) a))
(define (shift-l x m)
  (append x (zero-vector m))
     )
