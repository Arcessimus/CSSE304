(define Fahrenheit->Celsius
	(lambda (temperature)
		(/ (* 5 (- temperature 32)) 9)))
		
(define interval-contains?
	(lambda (interval number)
		(if (< number (car interval))
			#f
			(if (> number (cadr interval))
				#f
				#t))))

(define interval-intersects?
	(lambda (i1 i2)
		(if (interval-contains? i1 (car i2))
			#t
			(if (interval-contains? i2 (car i1))
			#t
			#f))))
			
(define interval-union
	(lambda (i1 i2)
		(if (interval-intersects? i1 i2)
			(list (list (min (car i1) (car i2)) (max (cadr i1) (cadr i2))))
			(list i1 i2))))
			
(define divisible-by-7?
	(lambda (num)
		(if (eq? (modulo num 7) 0)
			#t
			#f)))
			
(define ends-with-7?
	(lambda (num)
		(if (eq? (modulo (- num 7) 10) 0)
			#t
			#f)))
			
(define 1st
	(lambda (ls)
		(car ls)))
		
(define 2nd
	(lambda (ls)
		(cadr ls)))
		
(define 3rd
	(lambda (ls)
		(caddr ls)))