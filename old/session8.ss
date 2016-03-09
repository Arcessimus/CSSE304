(define contains?
	(lambda (slist sym)
		(let in-list? ([slist slist])
			(cond [(null? slist) #f]
				[(symbol? (car slist)) (or(eq?(car slist) sym)
											(int-list? (cdr slist)))]
				[else (or (in-list? (car slist)) (in-list? (cdr slist)))]))))
				
(define (count-occurrences slist sym)
	(let count ([slist slist])
		(cond [(null? slist) 0]
				[(symbol? (car slist)) (+ (if (eq? (car slist) sym)
												1
												0)
												(count (cdr slist)))]
				[else (+ (count (car slist)) (count (cdr slist))]))))
				
(define (flatten slist)
	(let flatten ([slist slist])
		(cond [(null? slist) '()]
				[(symbol? (car slist)) (cons (car slist)
										(flatten (cdr slist)))]
				[else (append(flatten (car slist)) (flatten (cdr slist)))])))