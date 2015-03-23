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
    (cond [(null? list) #t]
          [(contains? cdr(list) car(list)) #f]
          [else set? cdr(list)])))

(define contains?
  (lambda (list n)
    (cond [(null? list) #f]
          [eq? n car(list) #t]
          [else contains? cdr(list) n]))))
    
(define sum-of-squares
  (lambda (lon)
    (if (null? lon) 0)
    (+ (^ 2 car(lon)) sum-of-squares(cdr(lon)))))
  
(define make-vec-from-points
  (lambda (p1 p2)
    (vector ([- car(p1) car(p2)] [- cadr(p1) cadr(p2)] [- cddr(p1) cddr(p2)]))))
    
(define dot-product
  (lambda (v1 v2)
    (+ [* (vector-ref v1 0) (vector-ref v2 0)] + [* (vector-ref v1 1) (vector-ref v2 1)] [* (vector-ref v1 2) (vector-ref v2 2)])))
    
(define vec-length
  (lambda (v)
    sqrt(+ (^ (vector-ref v 0) 2) + (^ (vector-ref v 1) 2) (^ (vector-ref v 2) 2))))
    
(define distance
  (lambda (p1 p2)
    vec-length(make-vec-from-points(p1 p2))))
    
(define cross-product
  (lambda (v1 v2)
    
    
(define parallel?
  (lambda (v1 v2)
    (if (= arccos(/ dot-product(v1 v2) (* vec-length(v1) vec-length(v2))) 0) #t)
	(else #f)))
    
(define collinear?
  (lambda (p1 p2 p3)
    let v1 (make-vec-from-points(p1 p2))
	let v2 (make-vec-from-points(p1 p3))
	if (parallel?(v1 v2) #t)
	else (#f)))