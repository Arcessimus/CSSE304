;problem 1
(define interval-contains?
  (lambda (interval n)
    (cond[(< n (car interval)) #f]
          [(> n (cadr interval)) #f]
          [else #t])))

(define interval-intersects?
  (lambda (i1 i2)
    (cond [(interval-contains? i1 (car i2)) #t]
         [(interval-contains? i1 (cadr i2)) #t]
		 [(interval-contains? i2 (car i1)) #t]
		 [(interval-contains? i2 (cadr i1)) #t]
         [else #f])))

(define interval-union
  (lambda (i1 i2)
    (if (interval-intersects? i1 i2)
        (let ([x(min (car i1) (car i2))]
			[y (max (cadr i1) (cadr i2))])
         (list(list x y)))
    (list i1 i2))))

(define intersection-exists?
	(lambda (interval ls)
		(cond [(null? ls) #f]
			[(interval-intersects? interval (car ls)) #t]
			[else (intersection-exists? interval (cdr ls))])))	
	
(define reduce-list
	(lambda (interval ls original-list)
		(cond [(null? ls) original-list]
		[(interval-intersects? interval (car ls)) (append (interval-union interval (car ls)) (remove (car ls) (remove interval original-list)))]
		[else (reduce-list interval (cdr ls) original-list)])))
		
(define minimize-accum
	(lambda (ls minimized-list)
		(cond [(null? ls) ls]
			[(intersection-exists? (car ls) (cdr ls)) (minimize-accum ls (append (reduce-list (car ls) (cdr ls) ls) minimized-list))]
			[else (minimize-interval-list (cdr ls))])))

(define minimize-interval-list
	(lambda (ls)
		'()))

	
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
(define fact
  (lambda (n)
    (if (zero? n)
        1
        (* n (fact (- n 1))))))
		
(define choose
  (lambda (n k)
    (/ (fact n) (* (fact k) (fact(- n k))))))

(define make-triangle-row
	(lambda (n k row)
		(if (eq? n k) row
			(make-triangle-row n (+ k 1) (list row (list (choose n k)))))))

(define pascal-accum
	(lambda (n triangle-so-far)
		(if (zero? n) (append triangle-so-far (list 1))
			(pascal-accum (- n 1) (append triangle-so-far (make-triangle-row n 0 '()))))))

(define pascal-triangle
	(lambda (n)
		(if (< n 0) '()
			(pascal-accum n '()))))
	
;problem 5
(define make-pairs
	(lambda (element set)
		(cond [(null? set) '()]
			[else (list (make-pairs element (cdr set)) (list element (car set)))])))
			
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
(define make-list-of-vertices
	(lambda (G)
		(map car G)))
		
(define full-connection?
	(lambda (G vertex connections list-of-vertices)
		(cond [(null? list-of-vertices) #t]
			[(null? G) #f]
			[else 
			(let ([G2 (remove (list vertex connections) G)]
				[next-vertex (car connections)]
				[next-connections (remove vertex (cdr (find (equal? (caar G) (car connections)) connections)))]
				[remaining-vertices (remove vertex (remove connections list-of-vertices))])
				(full-connection? G2 next-vertex next-connections remaining-vertices))])))
		
(define check-connections
	(lambda (G vertex connections list-of-vertices)
		(cond [(null? list-of-vertices) #t]
			[(null? G) #f]
			[else 
			(let ([G2 (remove (list vertex connections) G)]
				[next-vertex (car connections)]
				[next-connections (remove vertex (cdr (find (equal? (caar G) (car connections)) connections)))]
				[remaining-vertices (remove vertex (remove connections list-of-vertices))])
				(if (not (full-connection? G2 next-vertex next-connections remaining-vertices)) #f
					(check-connections G (caadr G) (cdadr G) list-of-vertices)))])))
		
(define complete-helper
	(lambda (G list-of-vertices)
		(let ([x (caar G)]
			[connections (cadr G)])
			(check-connections G x connections list-of-vertices))))

(define complete?
	(lambda (G)
		(cond [(null? G) #t]
			[(eq? (length G) 1) #t]
			[else (complete-helper G (make-list-of-vertices G))])))
	
;problem 8
(define complete-accum
	(lambda (vertices remaining-vertices G)
		(cond [(null? remaining-vertices) G]
			[else (complete-accum vertices (cdr remaining-vertices) (append (list (car vertices)) (remove (car vertices) (vertices))))])))

(define complete
	(lambda (ls)
		(cond [(null? ls) '()]
		[else (complete-accum ls ls '())])))
	
;problem 9
(define contains?
	(lambda (lst obj)
		(cond [(null? lst) #f]
			[(equal? (car lst) obj) #t]
			[else (contains? (cdr lst) obj)])))
			
(define replace
	(lambda (old new ls)
		(cond [(null? ls) ls]
			[(not (contains? ls old)) ls]
			[(eq? (car ls) old) (append (list new) (replace old new (cdr ls)))]
			[else (append (list (car ls)) (replace old new (cdr ls)))])))
	
;problem 10
(define remove-first
	(lambda (element ls)
		(cond [(null? ls) ls]
			[(eq? (car ls) element) (append (cdr ls) '())]
			[else (append (list (car ls)) (remove-first element (cdr ls)))])))
	
;problem 11
(define remove-last
	(lambda (element ls)
		(cond [(null? ls) ls]
			[(and(eq? (car ls) element) (not(contains? (cdr ls) element))) (append (cdr ls) '())]
			[else (append (list (car ls)) (remove-last element (cdr ls)))])))