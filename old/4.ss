;problem 1
(define contains?
	(lambda (lst obj)
		(cond [(null? lst) #f]
			[(equal? (car lst) obj) #t]
			[else (contains? (cdr lst) obj)])))
			
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
			
(define check-element
	(lambda (obj)
		(cond [(not (symbol? (car obj))) #f]
			[(not (integer? (cadr obj))) #f]
			[(< (cadr obj) 0) #f]
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
			[(not (set? (map car obj))) #f]
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
			[(not (list? (car list-of-sublists))) #f]
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
			[comp-length (length (car list-of-sublists))])
			(check-lengths comp-length (cdr list-of-lengths)))))
			
(define matrix?
	(lambda (obj)
		(cond [(null? obj) #f]
			[(not (list? obj)) #f]
			[(not (correct-sublists? obj)) #f]
			[(eq? (length obj) 1) #t]
			[(not (equal-lengths? obj)) #f]
			[else #t])))
  
;problem 5
(define matrix-car
	(lambda (m)
		(map car m)))
	
(define matrix-cdr
	(lambda (m)
		(map cdr m)))

(define matrix-transpose-helper
	(lambda (oldm newm)
		(cond [(not (matrix? oldm)) newm]
			[else (matrix-transpose-helper (matrix-cdr oldm) (append newm (list(matrix-car oldm))))])))
			
(define matrix-transpose
	(lambda (m)
		(matrix-transpose-helper m '())))
  
;problem 6
(define last
	(lambda (lst)
		(cond [(null? (cdr lst)) (car lst)]
			[else (last (cdr lst))])))
  
;problem 7
(define all-but-last-helper
	(lambda (lst all-so-far)
		(cond[(null? (cdr lst)) all-so-far]
			[else (all-but-last-helper (cdr lst) (append all-so-far (list (car lst))))])))
			
(define all-but-last
	(lambda (lst)
		(cond [(null? (cdr lst)) '()]
			[else (all-but-last-helper (cdr lst) (list(car lst)))])))