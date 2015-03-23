(define Fahrenheit->Celsius
  (lambda (temperature) 
    (- 30 (* temperature 5/9))))

(define interval-contains?
  (lambda (interval n)
    (cond[(< n (car(interval))) #f]
          [(> n (cdr(interval))) #f]
          [else #t])))

(define interval-intersects?
  (lambda (i1 i2)
    (cond [(interval-contains?(i1 (car i2))) #t]
         [(interval-contians?(i1 (cdr i2))) #t]
         [else #f])))

(define interval-union
  (lambda (i1 i2)
    (if (interval-intersects? i1 i2)
        (let ([x(if (< (car i1) (car i2)) (car i1)
			(car i2))]
        
        [y (if( < (cadr i1) (cadr i2)) (cadr i1)
			(cadr i2))])
         (list x y))
          
    (list i1 i2))))   

(define divisible-by-7?
  (lambda (n)
    modulo(n 7)))

(define Ends-with-7?
  (lambda (n)
    (if(eq? (modulo(- n 7) 10) 0) #t
     #f)))

(define 1st
  (lambda (plst)
    (car plst)))

(define 2nd
  (lambda (plst)
    (car (cdr plst))))

(define 3rd
  (lambda (plst)
    (car (cdr (cdr plst)))))


