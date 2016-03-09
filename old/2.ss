; recursive factorial procedure
; parameter n: must be a non-zero integer

(define fact
  (lambda (n)
    (if (zero? n)
        1
        (* n (fact (- n 1))))))
		
(define choose
  (lambda (n k)
    (/ (fact n) (* (fact k) (fact(- n k))))))

(define range
  (lambda (m n)
    (if (< m n) (cons m (range (+ 1 m) n))
		'())))
    
(define contains?
  (lambda (list n)
    (cond [(null? list) #f]
          [(equal? n (car list)) #t]
          [else (contains? (cdr list) n)])))

(define set?
  (lambda (list)
    (cond [(null? list) #t]
          [(contains? (cdr list) (car list)) #f]
          [else (set? (cdr list))])))


    
(define sum-of-squares
  (lambda (lon)
    (if (null? lon) 0
    (+ (expt (car lon) 2) (sum-of-squares (cdr lon))))))
  
(define make-vec-from-points
  (lambda (p1 p2)
    (list [- (car p2) (car p1)] [- (cadr p2) (cadr p1)] [- (caddr p2) (caddr p1)])))
    
(define dot-product
  (lambda (v1 v2)
    (+ (+ [* (car v1) (car v2)] [* (cadr v1) (cadr v2)]) [* (caddr v1) (caddr v2)])))
    
(define vec-length
  (lambda (v)
    (sqrt(+ (expt (car v) 2) (+ (expt (cadr v) 2) (expt (caddr v) 2))))))
    
(define distance
  (lambda (p1 p2)
    (vec-length(make-vec-from-points p1 p2))))
    
(define cross-product
  (lambda (v1 v2)
	(let ([x1 (car v1)]
		[x2 (car v2)]
		[y1 (cadr v1)]
		[y2 (cadr v2)]
		[z1 (caddr v1)]
		[z2 (caddr v2)])
		(list [- (* y1 z2) (* y2 z1)] [* -1 (- (* x1 z2) (* x2 z2))] [- (* x1 y2) (* x2 y1)]))))
    
    
(define parallel?
  (lambda (v1 v2)
    (equal? (acos (/ (dot-product v1 v2) (* (vec-length v1) (vec-length v2)))) 0)))
    
(define collinear?
  (lambda (p1 p2 p3)
    (let ([v1 (make-vec-from-points p1 p2)]
		[v2 (make-vec-from-points p1 p3)])
	(parallel? v1 v2))))