;problem 1
;(define minimize-interval-list
	;(lambda (ls)))
	
;problem 2
(define exists?
	(lambda (pred ls)
		(ormap pred ls)))

;problem 3
(define find-index
	(lambda (pred ls current-index)
		(cond [(pred (car ls)) current-index]
			[else (find-index pred (cdr ls) (+ 1 current-index))])))
			
(define list-index
	(lambda (pred ls)
		(if (exists? pred ls) (find-index pred ls 0)
			#f)))		
			
;problem 4
(define pascal-triangle
	(lambda (n)
		))
	
;problem 5
(define make-pairs
	(lambda (element set)
		(cond [(null? set) '()]
			[else (append (make-pairs element (cdr set)) (list element (car set)))])))
			
(define product-help
	(lambda (set1 set2 product-so-far)
		(cond [(null? set1) product-so-far]
			[else (product-help (cdr set1) set2 (append product-so-far (make-pairs (car set1) set2)))])))
			
(define product
	(lambda (set1 set2)
		(product-help set1 set2 '())))
		
;problem 6
(define max-edges
	(lambda (n)
		/ (* n (- n 1)) 2))
		
;problem 7
;(define complete?
	;(lambda (G)))
	
;problem 8
;(define complete?
	;(lambda (ls)))
	
;problem 9
;(define replace
	;(lambda (old new ls)))
	
;problem 10
(define remove-first
	(lambda (element ls)
		(cond [(null? ls) ls]
			[(eq? (car ls) element) (append (cdr ls) '())]
			[else (append (remove-first element (cdr ls)) (list (car ls)))])))
	
;problem 11
;(define remove-last
	;(lambda (element ls)))