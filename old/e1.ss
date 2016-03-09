(define group-by-two
	(lambda (ls)
		(cond [(null? ls) '()]
			[(null? (cdr ls)) (car ls)]
			[else (list (list (car ls) (cadr ls)) (group-by-two (cddr ls)))])))
			
(define group-by-n
	(lambda (ls n)
		(cond [(null? ls) '()]
			)))
			
(define ms-add-one
	(lambda (ms sym)
		(cond [(null? ms) (list sym 1)]
			[(eq? (caar ms) sym) (list (list sym (+ 1 (cadar ms)))) (cdr ms)]
			[else (list (car ms) (ms-add-one (cdr ms) sym))])))
		
(define ms-diff
	(lambda (first second)
		(cond [(null? first) '()]
			[(equal? (caar first) (caar second)) 
				(if (> (cadar first) (cadar second)) (list (list (caar first) (- (cadar first) (cadar second))) (ms-diff (cdr first) (cdr second)))
					(list (ms-diff (cdr first) (cdr second))))]
			[else (list (cadr first) (cadr second))])))
			
(define ms-most-frequent
	(lambda (ms)
		(cond [(null? ms) #f])))
		
(define hailstone-sequence
	(lambda (n)
		(cond [(equal? n 1) '()]
			[(equal?(modulo n 2) 0) (list (hailstone-sequence (/ n 2)))]
			[(equal? (modulo n 2) 1) (list (hailstone-sequence(+ 1 (* 3 n))) n)])))
			
(define hailstone-max-height
	(lambda (n)
		(map max (hailstone-sequence n))))
		
(define list-of-maxes
	(lambda (low high)
		(if (<= low high) (list (hailstone-max-height low) (list-of-maxes (+ 1 low) high)))))
		
(define hailstone-range-max-height
	(lambda (low high)
		(max (list-of-maxes low high))))
		
