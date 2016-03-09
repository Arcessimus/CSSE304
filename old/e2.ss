(define vector-list
	(lambda (vect)
		(lambda (msg . args)
			(let ([vec-lst vect]
				[cur-size (vector-length vect)]
				[capac (vector-length vect)])
				
				(case msg
					[(full?) (eq? cur-size capac)]
					[(increase-capacity) (set! capac (* 2 capac)) 
						(let ([new-vec (make-vector capac)]
							[copy-old-vec 
								(lambda (old temp-size new)
									(if (< 0 temp-size)
										(copy-old-vec old (- temp-size 1)(vector-set! new temp-size (vector-ref old temp-size)))))])
							((copy-old-vec vec-lst (- cur-size 1) new-vec)))]
					[(capacity) capac]
					[(size) cur-size]
					[(get) (vector-ref vec-lst (car args))]
					[(add) (if (vector-list vec-lst 'full?) (vector-list vec-lst 'increase-capacity)) (vector-list vec-lst 'set cur-size (car args)) (set! cur-size (+ 1 cur-size))]
					[(remove) (set! cur-size (- cur-size 1))(vector-set! vec-lst cur-size '())]
					[(current) vec-lst]
					[(set) (vector-set! vec-lst (car args) (cadr args))])))))