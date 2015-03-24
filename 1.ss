(define Fahrenheit->Celsius
  (lambda (temperature) 
    (* 5/9 (- temperature 32))))

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

(define divisible-by-7?
  (lambda (n)
    (eq? (modulo n 7) 0)))

(define Ends-with-7?
  (lambda (n)
    (eq? (modulo (- n 7) 10) 0)))

(define 1st
  (lambda (plst)
    (car plst)))

(define 2nd
  (lambda (plst)
    (car (cdr plst))))

(define 3rd
  (lambda (plst)
    (car (cdr (cdr plst)))))


