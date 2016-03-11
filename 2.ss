;Derrick Milner Assignment 2

;#1
(define fact
  (lambda (n)
    (if (zero? n)
        1
        (* n (fact (- n 1))))))
		
(define choose
	(lambda (n k)
		(/ (fact n) (* (fact k) (fact (- n k))))))
		
;2
(define range
	(lambda (m n)
		(if (<= n m)
			'()
			(append (list m) (range (+ 1 m) n)))))
			
;3
(define list-contains?
	(lambda (item ls)
		(if (null? ls)
			#f
			(if (equal? item (car ls))
				#t
				(list-contains? item (cdr ls))))))
		
(define set?
	(lambda (ls)
		(if (null? ls)
			#t
			(if (list-contains? (car ls) (cdr ls))
				#f
				(set? (cdr ls))))))
				
;4
(define sum-of-squares-helper
	(lambda (lon acc)
		(if (null? lon)
			acc
			(sum-of-squares-helper (cdr lon) (+ acc (expt (car lon) 2))))))
			
(define sum-of-squares
	(lambda (lon)
		(sum-of-squares-helper lon 0)))

;5
(define make-vec-from-points
	(lambda (p1 p2)
		(list (- (car p2) (car p1)) (- (cadr p2) (cadr p1)) (- (caddr p2) (caddr p1)))))
		
;6
(define dot-product
	(lambda (v1 v2)
		(+ (* (car v1) (car v2)) (* (cadr v1) (cadr v2)) (* (caddr v1) (caddr v2)))))
		
;7
(define vec-length
	(lambda (v)
		(sqrt (+ (expt (car v) 2) (expt (cadr v) 2) (expt (caddr v) 2)))))
		
;8
(define distance
	(lambda (p1 p2)
		(vec-length (make-vec-from-points p1 p2))))
		
;9
(define cross-product
	(lambda (v1 v2)
		(list (- (* (cadr v1) (caddr v2)) (* (caddr v1) (cadr v2))) (- (* (caddr v1) (car v2)) (* (car v1) (caddr v2))) (- (* (car v1) (cadr v2)) (* (cadr v1) (car v2))))))
		
;10
(define parallel?
	(lambda (v1 v2)
		(if (equal? (cross-product v1 v2) '(0 0 0))
			#t
			#f)))
			
;11
(define collinear?
	(lambda (p1 p2 p3)
		(if (parallel? (make-vec-from-points p1 p2) (make-vec-from-points p1 p3))
			#t
			#f)))