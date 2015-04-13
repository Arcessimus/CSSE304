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
    

(define contains?
	(lambda (lst obj)
		(cond [(null? lst) #f]
			[(equal? (car lst) obj) #t]
			[else (contains? (cdr lst) obj)])))
; problem 2
(define union-accum
	(lambda (s2 union-so-far)
		(cond [(null? s2) union-so-far]
			[(contains? union-so-far (car s2)) (union-accum (cdr s2) union-so-far)]
			[else (union-accum (cdr s2) (append (list(car s2)) union-so-far))])))
	
(define union
  (lambda (s1 s2)
	(cond [(null? s1) s2]
		[(null? s2) s1]
		[else (union-accum s2 s1)])))
;problem 3
(define intersection-accum
	(lambda (s1 s2 intersection-so-far)
		(cond [(null? s1) intersection-so-far]
			[(contains? s2 (car s1)) (intersection-accum (cdr s1) s2 (append (list (car s1)) intersection-so-far))]
			[else (intersection-accum (cdr s1) s2 intersection-so-far)])))
		
(define intersection
  (lambda (s1 s2) 
	(cond [(or (null? s1) (null? s2)) '()]
		[else (intersection-accum s1 s2 '())])))
   
;problem 4   
(define subset?
  (lambda (s1 s2) 
	(cond [(null? s1) #t]
		[(contains? s2 (car s1)) (subset? (cdr s1) s2)]
		[else #f])))
		
;problem 5
(define set?
  (lambda (list)
    (cond [(null? list) #t]
          [(contains? (cdr list) (car list)) #f]
          [else (set? (cdr list))])))
		  
(define 2list?
	(lambda (lst)
		(cond [(null? lst) #f]
			[(not (list? lst)) #f]
			[(not (eq? (length lst) 2)) #f]
			[else #t])))
    
(define relation?
  (lambda (obj)
	(cond [(not (list? obj)) #f]
		[(null? obj) #t]
		[(not (set? obj)) #f]
		[else (andmap 2list? obj)])))
	
;problem 6
(define make-set
	(lambda (lst set-so-far)
		(cond [(null? lst) set-so-far]
			[(contains? set-so-far (car lst)) (make-set (cdr lst) set-so-far)]
			[else (make-set (cdr lst) (append (list (car lst)) set-so-far))])))

(define domain
  (lambda (rel)
	(make-set (map car rel) '())))
	
;problem 7
(define reflexive-pair?
	(lambda (lst)
		(equal? (car lst) (cadr lst))))
		
(define get-list-of-pairs
	(lambda (rel list-of-pairs)
		(cond [(null? rel) list-of-pairs]
			[(reflexive-pair? (car rel)) (get-list-of-pairs (cdr rel)(append (list (car rel)) list-of-pairs))]
			[else (get-list-of-pairs (cdr rel) list-of-pairs)])))
			
(define reflexive-helper
	(lambda (rel list-of-pairs)
		(cond [(null? rel) #t]
			[(contains? list-of-pairs (car rel)) (reflexive-helper (cdr rel) list-of-pairs)]
			[(not (contains? list-of-pairs (list (caar rel) (caar rel)))) #f]
			[(not (contains? list-of-pairs (list (cadar rel) (cadar rel)))) #f] 
			[else (reflexive-helper (cdr rel) list-of-pairs)])))
			
(define reflexive?
  (lambda (rel)
	(cond [(null? rel) #t]
		[else (reflexive-helper rel (get-list-of-pairs rel '()))])))
		
;problem 8
(define step-counter
	(lambda (n count)
		(cond [(eq? n 1) count]
			[(eq? (modulo n 2) 0) (step-counter (/ n 2) (+ 1 count))]
			[(eq? (modulo n 2) 1) (step-counter (+ 1 (* 3 n)) (+ 1 count))])))
    
(define hailstone-step-count 
  (lambda (n) 
	(cond [(eq? n 1) 0]
		[else (step-counter n 0)])))