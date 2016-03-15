;Derrick Milner Assignment 4

;1
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
			
(define multi-set-item?
	(lambda (list-obj)
		(and (symbol? (car list-obj)) (and (number? (cadr list-obj)) (positive? (cadr list-obj))))))

(define list-contains?
	(lambda (item ls)
		(if (null? ls)
			#f
			(if (equal? item (car ls))
				#t
				(list-contains? item (cdr ls))))))
				
(define contains-duplicates?
	(lambda (ls)
		(cond
			[(null? ls) #f]
			[(list-contains? (car ls) (cdr ls)) #t]
			[else (contains-duplicates? (cdr ls))])))
			
(define multi-set?
	(lambda (obj)
		(cond
			[(null? obj) #t]
			[(not (list-of-2-lists? obj)) #f]
			[(list-contains? #f(map multi-set-item? obj)) #f]
			[else (not (contains-duplicates? (map car obj)))])))
			
;2
(define ms-size
	(lambda (ms)
		(apply + (map cadr ms))))
		
;3
(define matrix-ref
	(lambda (m row col)
		(list-ref (list-ref m row) col)))
		
;4
(define list-of-numbers?
	(lambda (ls)
		(cond
			[(null? ls) #f]
			[(not (list? ls)) #f]
			[else (and (map number? ls))])))
			
(define check-lengths ;from old code
	(lambda (comp-length list-of-lengths)
		(cond [(null? list-of-lengths) #t]
			[(eq? comp-length (car list-of-lengths)) (check-lengths comp-length (cdr list-of-lengths))]
			[else #f])))
			
(define equal-lengths? ;from old code
	(lambda (list-of-sublists)
		(let ([list-of-lengths (map length list-of-sublists)]
			[comp-length (length (car list-of-sublists))])
			(check-lengths comp-length (cdr list-of-lengths)))))
			
(define matrix?
	(lambda (m)
		(cond
			[(null? m) #f]
			[(not (list? m)) #f]
			[(list-contains? #f(map list-of-numbers? m)) #f]
			[else (equal-lengths? m)])))
			
;5
(define matrix-car ;old code
	(lambda (m)
		(map car m)))
	
(define matrix-cdr ;old code
	(lambda (m)
		(map cdr m)))
		
(define transpose-help
	(lambda (m new-m)
		(cond
			[(not (matrix? m)) new-m]
			[else (transpose-help (matrix-cdr m) (append new-m (list (matrix-car m))))])))
			
(define matrix-transpose
	(lambda (m)
		(transpose-help m '())))
		
;6
(define last
	(lambda (ls)
		(cond
			[(null? (cdr ls)) (car ls)]
			[else (last (cdr ls))])))
			
;7 - i'm tired and up past my normal bed time, which I'm trying to keep up so that I can learn more effectively so this is also from old code, but if 
; I was able to get this last year with my limited knowledge of scheme, and understand it now, then I am satisfied with that
(define all-but-last-helper
	(lambda (lst all-so-far)
		(cond[(null? (cdr lst)) all-so-far]
			[else (all-but-last-helper (cdr lst) (append all-so-far (list (car lst))))])))
			
(define all-but-last
	(lambda (lst)
		(cond [(null? (cdr lst)) '()]
			[else (all-but-last-helper (cdr lst) (list(car lst)))])))