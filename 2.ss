; recursive factorial procedure
; parameter n: must be a non-zero integer

(define fact
  (lambda (n)
    (if (zero? n)
        1
        (* n (fact (- n 1))))))
		
(define choose
  (lambda (n k)
    (/ fact(n) (* fact(k) fact(- n k)))))

(define range
  (lambda (m n)
    (if (< m n)
      cons(m (range((+ 1 m) n))))
    (else '())))
    
(define set?
  (lambda (list)
    [cond ((null? list) #t)
          ((contains? cdr(list) car(list)) #f)
          (else set? cdr(list))]))

(define contains?
  (lambda (list n)
    [cond ((null? list) #f)
          (= n car(list) #t)
          (else contains? cdr(list) n)]))
    
(define sum-of-squares
  (lambda (lon)
    (if (null? lon) 0)
    (+ (^ 2 car(lon)) sum-of-squares(cdr(lon)))))
  
(define make-vec-from-points
  (lambda (p1 p2)
    
(define dot-product
  (lambda (v1 v2)
    
(define vec-length
  (lambda (v)
    
(define distance
  (lambda (p1 p2)
    vec-length(make-vec-from-points(p1 p2))))
    
(define cross-product
  (lambda (v1 v2)
    
(define parallel?
  (lambda (v1 v2)
    
(define collinear?
  (lambda (p1 p2 p3)