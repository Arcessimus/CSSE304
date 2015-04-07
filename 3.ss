; problem 1
(define make-vec-from-points
  (lambda (p1 p2)
    (list [- (car p2) (car p1)] [- (cadr p2) (cadr p1)] [- (caddr p2) (caddr p1)])))

(define vec-length
  (lambda (v)
    (sqrt(+ (expt (car v) 2) (+ (expt (cadr v) 2) (expt (caddr v) 2))))))

(define distance
  (lambda (p1 p2)
    (vec-length(make-vec-from-points p1 p2))))
	
(define nearest-point-helper
	(lambda (p list-of-points nearest-so-far shortest-distance-so-far)
		(cond [(null? list-of-points) nearest-so-far]
			[(> shortest-distance-so-far (distance p (car list-of-points))) (nearest-point-helper p (cdr list-of-points) (car list-of-points) (distance p (car list-of-points)))]
			[else (nearest-point-helper p (cdr list-of-points) nearest-so-far shortest-distance-so-far)])))
	
(define nearest-point
  (lambda (p list-of-points)
	(nearest-point-helper p list-of-points (car list-of-points) (distance p (car list-of-points)))))
    
; problem 2
(define union-accum
	(lambda (s1 s2 union-so-far)
		(cond [(null? s1) ]
			[])))
	
(define union
  (lambda (s1 s2)
	(cond [(null? s1) s2]
		[(null? s2) s1]
		[else (union-accum s1 s2 '())])))
;problem 3
	
(define intersection
  (lambda (s1 s2) 
	(cond [(or (null? s1) (null? s2)) '()])))
    
;(define subset?
 ; (lambda (s1 s2) ))
    
;(define relation?
 ; (lambda (obj)  ))
    
;(define domain
 ; (lambda (rel)  ))
    
;(define reflexive?
 ; (lambda (rel) ))
    
;(define hailstone-step-count 
 ; (lambda (n) ))