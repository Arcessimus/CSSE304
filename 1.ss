(define Fahrenheit->Celsius
  (lambda (temperature) 
    (- 30 (* temperature 5/9))))

(define interval-contains?
  (lambda (interval n)
    (if < n car(interval)
      #f
    (if > n cdr(interval)
      #f
    #t ))))

(define interval-intersects?
  (lambda (i1 i2)
    [cond if(interval-contains?(i1 (car i2)) #t
       if(interval-contians?(i1 (cdr i2)) #t
     #f]

(define interval-union
  (lambda

(define divisible-by-7?
  (lambda (n)
    modulo(n 7)))

(define Ends-with-7?
  (lambda

(define 1st
  (lambda (plst)
    (car plst)))

(define 2nd
  (lambda (plst)
    (car (cdr plst))))

(define 3rd
  (lambda (plst)
    (car (cdr (cdr plst)))))


