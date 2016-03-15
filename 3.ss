;Assignment 3 - Derrick Milner

;vector procedures first
(define make-vec-from-points
	(lambda (p1 p2)
		(list (- (car p2) (car p1)) (- (cadr p2) (cadr p1)) (- (caddr p2) (caddr p1)))))
		
(define dot-product
	(lambda (v1 v2)
		(+ (* (car v1) (car v2)) (* (cadr v1) (cadr v2)) (* (caddr v1) (caddr v2)))))
		
(define vec-length
	(lambda (v)
		(sqrt (+ (expt (car v) 2) (expt (cadr v) 2) (expt (caddr v) 2)))))
		
(define distance
	(lambda (p1 p2)
		(vec-length (make-vec-from-points p1 p2))))
		
(define cross-product
	(lambda (v1 v2)
		(list (- (* (cadr v1) (caddr v2)) (* (caddr v1) (cadr v2))) (- (* (caddr v1) (car v2)) (* (car v1) (caddr v2))) (- (* (car v1) (cadr v2)) (* (cadr v1) (car v2))))))
		
(define parallel?
	(lambda (v1 v2)
		(if (equal? (cross-product v1 v2) '(0 0 0))
			#t
			#f)))
			
(define collinear?
	(lambda (p1 p2 p3)
		(if (parallel? (make-vec-from-points p1 p2) (make-vec-from-points p1 p3))
			#t
			#f)))
			
;1
(define nearest-point-acc
	(lambda (p list-of-points min-point min-distance)
		(if (null? list-of-points)
			min-point
			(if (< (distance p (car list-of-points)) min-distance)
				(nearest-point-acc p (cdr list-of-points) (car list-of-points) (distance p (car list-of-points)))
				(nearest-point-acc p (cdr list-of-points) min-point min-distance)))))
				
(define nearest-point
	(lambda (p list-of-points)
		(if (null? list-of-points)
			'()
			(nearest-point-acc p (cdr list-of-points) (car list-of-points) (distance p (car list-of-points))))))
			
;2 - I referenced my old code for this one. 
(define set-contains?
	(lambda (item ls)
		(if (null? ls)
			#f
			(if (equal? item (car ls))
				#t
				(set-contains? item (cdr ls))))))
				
				
(define union-acc
	(lambda (set2 acc)
		(if (null? set2)
			acc
			(if (set-contains? (car set2) acc)
				(union-acc (cdr set2) acc)
				(union-acc (cdr set2) (append acc (list (car set2))))))))
				
(define union
	(lambda (set1 set2)
		(if (null? set1)
			set2
			(if (null? set2)
				set1
				(union-acc set2 set1)))))
				
;3
(define intersection-helper
	(lambda (s1 s2 intersection-so-far)
		(if (null? s1)
			intersection-so-far
			(if (set-contains? (car s1) s2)
				(intersection-helper (cdr s1) s2 (append intersection-so-far (list (car s1))))
				(intersection-helper (cdr s1) s2 intersection-so-far)))))
				
(define intersection
	(lambda (s1 s2)
		(if (or (null? s1) (null? s2))
			'()
			(intersection-helper s1 s2 '()))))
			
;4
(define subset?
	(lambda (s1 s2)
		(if (null? s1)
			#t
			(if (not (set-contains? (car s1) s2))
				#f
				(subset? (cdr s1) s2)))))
				
;5
(define set?
	(lambda (ls)
		(cond
			[(not (list? ls)) #f]
			[(null? ls) #t]
			[(set-contains? (car ls) (cdr ls)) #f]
			[else (set? (cdr ls))])))
			
(define list-size
	(lambda (ls cur-size)
		(if (null? ls)
			cur-size
			(list-size (cdr ls) (+ 1 cur-size)))))
			
(define list-of-2-lists?
	(lambda (ls)
		(cond
			[(null? ls) #t]
			[(not (list? (car ls))) #f]
			[(not (equal? (list-size (car ls) 0) 2)) #f]
			[else (list-of-2-lists? (cdr ls))])))
			
(define relation?
	(lambda (ls)
		(cond
			[(not (list? ls)) #f]
			[(not (set? ls)) #f]
			[(not (list-of-2-lists? ls)) #f]
			[else #t])))
			
;6
(define domain-acc
	(lambda (ls acc)
		(cond
			[(null? ls) acc]
			[(set-contains? (caar ls) acc) (domain-acc (cdr ls) acc)]
			[else (domain-acc (cdr ls) (cons (caar ls) acc))])))
			
(define domain
	(lambda (ls)
		(if (null? ls)
			'()
			(domain-acc ls '()))))
			
;7 - did check old code for hint
(define reflexive-pair?
	(lambda (lst)
		(equal? (car lst) (cadr lst)))) ;from old code
		
(define get-list-of-pairs
	(lambda (rel list-of-pairs)
		(cond [(null? rel) list-of-pairs]
			[(reflexive-pair? (car rel)) (get-list-of-pairs (cdr rel)(append (list (car rel)) list-of-pairs))]
			[else (get-list-of-pairs (cdr rel) list-of-pairs)]))) ;from old code
			
(define domain-in-reflexive-pairs
	(lambda (set-domain possible)
		(cond
			[(null? set-domain) #t]
			[(set-contains? (car set-domain) possible) (domain-in-reflexive-pairs (cdr set-domain) possible)]
			[else #f])))
			
(define range-acc
	(lambda (ls acc)
		(cond
			[(null? ls) acc]
			[(set-contains? (cadar ls) acc) (range-acc (cdr ls) acc)]
			[else (range-acc (cdr ls) (cons (cadar ls) acc))])))
			
(define range
	(lambda (ls)
		(if (null? ls)
			'()
			(range-acc ls '()))))
		
(define reflexive?
	(lambda (ls)
		(cond
			[(null? ls) #t]
			[(not (domain-in-reflexive-pairs (domain ls) (map car (get-list-of-pairs ls '())))) #f]
			[(not (domain-in-reflexive-pairs (range ls) (map car (get-list-of-pairs ls '())))) #f]
			[else #t])))
			
;8
(define hailstone-step-count-acc
	(lambda (n acc)
		(cond
			[(<= n 1) acc]
			[(eq? (modulo n 2) 0) (hailstone-step-count-acc (/ n 2) (+ acc 1))]
			[else (hailstone-step-count-acc (+ (* n 3) 1) (+ acc 1))])))
			
(define hailstone-step-count
	(lambda (n)
		(if (<= n 1)
			0
			(hailstone-step-count-acc n 0))))