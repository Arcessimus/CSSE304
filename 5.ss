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
;(define pascal-triangle
	;(lambda (n)))
	
;problem 5
(define product
	(lambda (set1 set2)
		())