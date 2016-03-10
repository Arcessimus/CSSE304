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
