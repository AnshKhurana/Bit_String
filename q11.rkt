#lang racket
;;;;;;;;;;;;;Q11;;;;;;;;;;;;;;;
(define (mult k l a b)
  (let* (
         [e-a (cadr a)]
         [e-b (cadr b)]
         [f-a (caddr a)]
         [f-b (caddr b)]
         [s-a (if (= (car a) 1) #f #t)]
         [s-b (if (= (car b) 1) #f #t)]
         [sign (if (and s-a s-b) 0 1)]
         [bias (- (expt 2 (- k 1)) 1)]
         [bias-string (u2b k bias)]
         
         
         [maxexp (u2b k (- (expt 2 k) 2))]
         
         )
    (cond [(and (equal? e-a (zero-vector k)) (equal? e-b (zero-vector k)))
           (cons sign (zero-float k l))]
          [(equal? e-a (zero-vector k)) (cons sign (convert-mult-under (u-mult (+ l 1) (cons 0 f-a) (cons 1 f-b))
                                                                    (zero-add 1 e-a) (zero-add 1 e-b) (zero-add 1 bias-string) k l))]
          [(equal? e-b (zero-vector k)) ( cons sign(convert-mult-under (u-mult (+ l 1) (cons 1 f-a) (cons 0 f-b))
                                                                    (zero-add 1 e-a) (zero-add 1 e-b) (zero-add 1 bias-string) k l))]
          [else (cons sign (convert-mult (u-mult (+ l 1) (cons 1 f-a) (cons 1 f-b))
                                         (zero-add 1 e-a) (zero-add 1 e-b) (zero-add 1 bias-string)
                                         maxexp k l))])
    ))
(define (convert-mult-under bitstring e-a e-b bias k l)
  (let* ( [pos (pos-one bitstring)]
          [non-zero-exp (if (equal? e-a (zero-vector (+ k 1))) e-b e-a)]
          [expnormal (u-sub (+ k 1) (u-add (+ k 1) non-zero-exp (u2b (+ k 1) 1)) bias)] 
          [res-exp (u-sub (+ k 1) (u-add (+ k 1 ) expnormal (u2b (+ k 1) 2)) (u2b (+ k 1) pos))]
          [bias-1  (b2u (+ k 1) (u-sub (+ k 1) bias (u2b (+ k 1) 1)))]
          [factor (- (b2u (+ k 1) non-zero-exp) (b2u (+ k 1) bias))]
          )
  (cond [(equal? res-exp (zero-vector (+ k 1))) (cons (cdr res-exp) (list (slice bitstring (+ 3 factor)  (+ 3 factor l))))]
        [(cons (cdr res-exp) (list (slice bitstring (+ pos 1) (+ pos l))))])))

      
(define (zero-float k l)
  (cons (zero-vector k) (list (zero-vector l))))

(define (convert-mult bitstring e-a e-b bias maxexp k l)
  (let* ( [ resexp0 (u-sub (+ l 1) (u-add (+ l 1) e-a e-b) bias)]
        [ resexp1 (u-sub (+ l 1) (u-add (+ l 1) (u-add (+ l 1) e-a e-b) (u2b (+ k 1) 1)) bias)]
    )
  (cond [(= (car bitstring) 0) (if (check (cons 0 maxexp) resexp0)
                                   (max-float k l)
                                   (cons (cdr resexp0) (list (slice bitstring ( + (pos-one bitstring) 1) (+ l (pos-one bitstring))))))]
        [else (if (check (cons 0 maxexp) resexp1) (max-float maxexp l) (cons (cdr resexp1) (list (slice bitstring ( + (pos-one bitstring) 1) (+ l  (pos-one bitstring) )))))]
        )))
(define (max-float maxexp l)
  (cons maxexp (list (make-list l 1))))

 
(define (divide k l a b)
  (let* (
         [e-a (cadr a)]
         [e-b (cadr b)]
         [f-a (caddr a)]
         [f-a-d (if (equal? e-a (zero-vector k))
                    (cons 0 (shift-l f-a (+ 1 l)))
                    (cons 1 (shift-l f-a (+ 1 l))))]
         [f-b (caddr b)]
         [f-b-d (if (equal? e-b (zero-vector k))
                    (zero-add (+ 2 l) f-b)
                    (zero-add (+ 1 l) (cons 1 f-b)))]
         
         [s-a (if (= (car a) 1) #f #t)]
         [s-b (if (= (car b) 1) #f #t)]
         [sign (if (and s-a s-b) 0 1)]
         [bias (- (expt 2 (- k 1)) 1)]
         [bias-string (u2b (+ k 1) bias)]
         [maxexp (u2b (+ k 1) (- (expt 2 k) 2))]
         [n (+ (* 2 l) 2)]
         
         )
     (cond [(and (equal? e-a (zero-vector k)) (equal? e-b (zero-vector k)))
                                                     (cons sign (convert-div-both-under (car (u-div n f-a-d f-b-d))
                                                               (zero-add 1 e-a) (zero-add 1 e-b) bias-string maxexp k l))]
           [(equal? e-a (zero-vector k)) (cons sign (convert-div-under (car (u-div n f-a-d f-b-d))
                                                                    (zero-add 1 e-a) (zero-add 1 e-b) bias-string maxexp k l))]
           [(equal? e-b (zero-vector k)) (cons sign (convert-div-under (car (u-div n f-a-d f-b-d))
                                                                    (zero-add 1 e-a) (zero-add 1 e-b)  bias-string maxexp k l))]
           [else (cons sign (convert-div (car (u-div n f-a-d f-b-d))
                                          (zero-add 1 e-a) (zero-add 1 e-b) bias-string
                                          maxexp k l))]
    )))

(define (convert-div bitstring e-a e-b bias-string maxexp k l)
  (let* ( [pos-1 (pos-one bitstring)]
          [ideal-exp (u-sub (+ k 1) (u-add (+ k 1) e-a bias-string) e-b)]
          [factor (- (+ 1 l) pos-1 )]
          [res-exp (if (>= factor 0)
                       (u-add (+ k 1) ideal-exp (u2b (+ k 1) factor))
                       (u-sub (+ k 1) ideal-exp (u2b (+ k 1) (- factor)))
                       )]
          [x (b2u (+ k 1) (u-sub (+ k 1) (u-add (+ k 1) e-b (u2b (+ k 1) 1))
                         (u-add (+ k 1) e-a bias-string))) ] 
          
)
    (cond [(check maxexp res-exp) (max-float k l)]
          [(equal? res-exp (zero-vector (+ k 1))) (cons (cdr res-exp) (if (>  x (+ l 3)) (list (zero-vector l)) 
                                                                          (list (slice bitstring (+ 3 l (- x)) (+ 2 (* 2 l) (- x))))))]  
          [else (cons (cdr res-exp) (list (slice bitstring (+ pos-1 1) (+ pos-1 l))))]
          )))
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;         
(define (convert-div-under bitstring e-a e-b bias-string maxexp k l)
  (let* ( [pos-1 (pos-one bitstring)]
          [ideal-exp (if (equal? e-a (zero-vector (+ k 1)) )
                        (u-sub (+ k 1) (u-add (+ k 1) bias-string (u2b (+ k 1) 1)) e-b)
                        (u-sub (+ k 1) (u-add (+ k 1) e-a bias-string) (u2b (+ k 1) 1))
                        ) ]
          [factor (- (+ 1 l) pos-1 )]
          [res-exp (if (>= factor 0)
                       (u-add (+ k 1) ideal-exp (u2b (+ k 1) factor))
                       (u-sub (+ k 1) ideal-exp (u2b (+ k 1) (- factor)))
                       )]
          [x (b2u (+ k 1) (u-sub (+ k 1) (u-add (+ k 1) e-b (u2b (+ k 1) 1))
                         (u-add (+ k 1) e-a bias-string))) ] 
          
)
    (cond [(check maxexp res-exp) (max-float k l)]
          [(equal? res-exp (zero-vector (+ k 1))) (cons (cdr res-exp) (if (>  x (+ l 3)) (list (zero-vector l)) 
                                                                          (list (slice bitstring (+ 3 l (- x)) (+ 2 (* 2 l) (- x))))))]  
          [else (cons (cdr res-exp) (list (slice bitstring (+ 1 pos-1) (+ pos-1 l))))]
          )))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (convert-div-both-under bitstring e-a e-b bias-string maxexp k l)
  (let* ( [pos-1 (pos-one bitstring)]
          [ideal-exp bias-string]
          [factor (- (+ 1 l) pos-1 )]
          [res-exp (if (>= factor 0)
                       (u-add (+ k 1) ideal-exp (u2b (+ k 1) factor))
                       (u-sub (+ k 1) ideal-exp (u2b (+ k 1) (- factor)))
                       )]
          [x (b2u (+ k 1) (u-sub (+ k 1) (u-add (+ k 1) e-b (u2b (+ k 1) 1))
                         (u-add (+ k 1) e-a bias-string))) ] 
          
)
    (cond [(check maxexp res-exp) (max-float k l)]
          [(equal? res-exp (zero-vector (+ k 1))) (cons (cdr res-exp) (if (>  x (+ l 3)) (list (zero-vector l)) 
                                                                          (list  (slice bitstring (+ 3 l (- x)) (+ 2 (* 2 l) (- x))))))]  
          [else (cons (cdr res-exp) (list (slice bitstring (+ pos-1 1) (+ pos-1 l))))]
          )))
                              

          

;;;;;;;;;;;;;xxx;;;;;;;;;;;;;;;
(define (slice l i k)
   (slice-h l '() 1 i k)
    )
 (define (slice-h l r c i k)
   (cond [(null? l) (reverse r)]
         [(or (< c i) (> c k)) (slice-h (cdr l) r (+ c 1) i k)]
         [else (slice-h (cdr l) (cons (car l) r) (+ c 1) i k)]
         ))

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

(define (check a b)
 
  (cond [(null? a) #t]
        [(equal? a b) #f]
        [ (= (car a) 0) (if (= (car b) 0) (check (cdr a) (cdr b) ) #t)]
        [else (if (= (car b) 1) (check (cdr a) (cdr b)) #f)]))
(define (zero-vector n)
  
      (make-list n 0)
        )
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



