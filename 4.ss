;problem 1
(define 2list?
	(lambda (lst)
		(cond [(null? lst) #f]
			[(not (list? lst)) #f]
			[(not (eq? (length lst) 2)) #f]
			[else #t])))
			
(define check-element
	(lambda (obj)
		(cond [(not (symbol? (car lst))) #f]
			[(not (integer? (cadr lst))) #f]
			[else #t])))
			
(define check-elements
	(lambda (lst)
		(cond [(null? lst) #t]
			[(not (check-element(car lst))) #f]
			[else (check-elements (cdr lst))])))
			
(define multi-set?
	(lambda (obj)
		(cond [(null? obj) #t]
			[(not (list? obj)) #f]
			[(not (andmap 2list? obj)) #f]
			[else (check-elements obj)])))
			
;problem 2
(define ms-size
	(lambda (ms)
		(apply + (map cadr ms))))
  
;problem 3
(define get-row
	(lambda (m row)
		(cond [(eq? row 0) (car m)]
			[else (get-row (cdr m) (- row 1))])))
			
(define get-col-num
	(lambda (row col)
		(cond [(eq? col 0) (car row)]
			[else (get-col-num (cdr row) (- col 1))])))
			
(define matrix-ref
	(lambda (m row col)
		(get-col-num (get-row m row) col)))
		
;problem 4
(define list-of-numbers?
	(lambda (lst)
		(cond [(null? lst) #t]
			[(not (integer? (car lst))) #f]
			[else (list-of-numbers? (cdr lst))])))
			
(define correct-sublists?
	(lambda (list-of-sublists)
		(cond [(null? list-of-sublists) #t]
			[(null? (car list-of-sublists)) #f]
			[(list-of-numbers? (car list-of-sublists)) (correct-sublists? (cdr list-of-sublists))])))
			
(define check-lengths
	(lambda (comp-length list-of-lengths)
		(cond [(null? list-of-lengths) #t]
			[(eq? comp-length (car list-of-lengths)) (check-lengths comp-length (cdr list-of-lengths))]
			[else #f])))
			
(define equal-lengths?
	(lambda (list-of-sublists)
		(let ([list-of-lengths (map length list-of-sublists)]
			[comp-length (car list-of-lengths)])
			(check-lengths comp-length (cdr list-of-lengths)))))
			
(define matrix?
	(lambda (obj)
		(cond [(null? obj) #f]
			[(not (list? obj)) #f]
			[(not (correct-sublists? obj)) #f]
			[(not (equal-lengths? obj)) #f]
			[else #t])))
  
;problem 5
(define matrix-transpose-helper
	(lambda (oldm newm)
		(cond [(not (matrix? oldm))])))
(define matrix-transpose
	(lambda (m)
		))
  
;problem 6
(define last
	(lambda (lst)
		(cond [(null? (cdr lst)) (car lst)]
			[else (last (cdr lst))])))
  
;problem 7
(define all-but-last-helper
	(lambda (lst all-so-far)
		(cond[(null? (cdr lst)) all-so-far]
			[else (all-but-last-helper (cdr lst) (append (list (car lst)) all-so-far))])))
			
(define all-but-last
	(lambda (lst)
		(cond [(null? (cdr lst)) '()]
			[else (all-but-last-helper (cdr lst) (car lst))])))