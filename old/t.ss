;; Test code for CSSE 304 Exam 2 201530

(define (test-vector-list)
    (let ([correct '(
		     (3 3 6)
		     (3 3 a)
		     (a 6)
		     (4 6 6 7)
		     (2 3 5 6)
		     (7 12 50)
		     )]
          [answers 
            (list 
	     (let () 
	       (define v '#(1 5 6)) 
	       (define vl (vector-list v)) 
	       (list (vl 'size) 
		     (vl 'capacity) 
		     (vl 'get 2)) )
	     (let () 
	       (define v '#(0 5 6)) 
	       (define vl (vector-list v)) 
	       (vl 'set 2 'a) 
	       (list (vl 'size) (vl 'capacity) (vl 'get 2)) )
	     (let () 
	       (define v '#(2 5 6)) 
	       (define vl (vector-list v)) 
	       (define vl2 (vector-list v)) 
	       (vl 'set 2 'a) 
	       (list (vl 'get 2) (vl2 'get 2)) )
	     (let () (define v '#(3 5 6)) 
		    (define vl (vector-list v)) 
		    (vl 'add 7) 
		    (list (vl 'size) (vl 'capacity) (vl 'get 2) (vl 'get 3)) )
	     (let () 
	       (define v '#(4 5 6)) 
	       (define vl (vector-list v)) 
	       (let ([a (vl 'remove)]) 
		 (list (vl 'size) (vl 'capacity) (vl 'get 1) a)) )
	     (let ()
	       (define v '#(6 5 6))
	       (define vl (vector-list v)) 
	       (vl 'remove) 
	       (vl 'add 10) 
	       (vl 'add 20) 
	       (vl 'add 30) 
	       (vl 'remove) 
	       (vl 'add 40) 
	       (vl 'add 50) 
	       (vl 'add 60) 
	       (list (vl 'size) (vl 'capacity) (vl 'get 5)) )
	     )])
      (display-results correct answers equal?)))

(define (test-when)
    (let ([correct '(
		     ((10) (3))
		     ((10 7) (3 4))
)]
          [answers 
            (list 
	     (list 
	      (eval-one-exp ' (let ([a 3] 
				    [b (list 4)]) 
				(when (< a 5) 
				  (set-car! b (+ (car b) a)) 
				  (set-car! b (+ (car b) a))) 
				b)) 
	      (eval-one-exp ' (let ([a 3] 
				    [b (list 3)]) 
				(when (> a 5) 
				  (set-car! b (+ (car b) a)) 
				  (set-car! b (+ (car b) a))) 
				b)))
	     (list (eval-one-exp ' 
		    (let ([a 3] 
			  [b (list 4 5)]) 
		      (when (< a 5) 
			(set-car! (cdr b) (+ (car b) a)) 
			(set-car! b (+ (car b) a))) 
		      (set-car! b (+ (car (cdr b)) a)) 
		      b)) 
		   (eval-one-exp ' 
		    (let ([a 3] 
			  [b (list 3 4)]) 
		      (when (> a 5) 
			(set-car! (cdr b) b (+ (car b) a)) 
			(set-car! b (+ (car b) a))) b)))
	     )])
      (display-results correct answers equal?)))


;-----------------------------------------------

(define display-results
  (lambda (correct results test-procedure?)
     (display ": ")
     (pretty-print 
      (if (andmap test-procedure? correct results)
          'All-correct
          `(correct: ,correct yours: ,results)))))


(define sequal?-grading
  (lambda (l1 l2)
    (cond
     ((null? l1) (null? l2))
     ((null? l2) (null? l1))
     ((or (not (set?-grading l1))
          (not (set?-grading l2)))
      #f)
     ((member (car l1) l2) (sequal?-grading
                            (cdr l1)
                            (rember-grading
                             (car l1)
                             l2)))
     (else #f))))

(define set?-grading
  (lambda (s)
    (cond [(null? s) #t]
          [(not (list? s)) #f]
          [(member (car s) (cdr s)) #f]
          [else (set?-grading (cdr s))])))

(define rember-grading
  (lambda (a ls)
    (cond
     ((null? ls) ls)
     ((equal? a (car ls)) (cdr ls))
     (else (cons (car ls) (rember-grading a (cdr ls)))))))

(define set-equals? sequal?-grading)

(define find-edges  ; e know that this node is in the graph before we do the call
  (lambda (graph node)
    (let loop ([graph graph])
      (if (eq? (caar graph) node)
	  (cadar graph)
	  (loop (cdr graph))))))

;; Problem 8  graph?
(define set?  ;; Is this list a set?  If not, it is not a graph.
  (lambda (list)
    (if (null? list) ;; it's an empty set.
	#t
	(if (member (car list) (cdr list))
	    #f
	    (set? (cdr list))))))


(define graph?
  (lambda (obj)
    (and (list? obj)
	 (let ([syms (map car obj)])
	   (and (set? syms)
		(andmap symbol? syms)
		(andmap (lambda (x)
			  (andmap (lambda (y) (member y (remove (car x) syms)))
				  (cadr x)))
			obj))))))
    
(define graph-equal?
  (lambda (a b)
    (and
     (graph? a) 
     (graph? b)
     (let ([a-nodes (map car a)]
	   [b-nodes (map car b)])
       (and 
	(set-equals? a-nodes b-nodes)
	    ; Now  See if the edges from each node are equivalent in the two graphs.
	(let loop ([a-nodes a-nodes])
	  (if (null? a-nodes)
	      #t
	      (let ([a-edges (find-edges a (car a-nodes))]
		    [b-edges (find-edges b (car a-nodes))])
		(and (set-equals? a-edges b-edges)
		     (loop (cdr a-nodes)))))))))))

(define (test-graph-equal)
  (list
   (graph-equal? '((a (b)) (b (a))) '((b (a)) (a (b))))
   (graph-equal? '((a (b c d)) (b (a c d)) (c (a b d)) (d (a b c)))
		 '((b (a c d)) (c (a b d)) (a (b d c)) (d (b a c))))
   (graph-equal? '((a ())) '((a ())))
   (graph-equal? '((a (b c)) (b (a c)) (c (a b))) '((a (b c)) (b (a c)) (c (a b))))
   (graph-equal? '() '())
   ))



(define g test-graph-equal)
	   
	  
     



;You can run the tests individually, or run them all
;#by loading this file (and your solution) and typing (r)

(define (run-all)
  (display 'vector-list) 
  (test-vector-list )
  (display 'when) 
  (test-when)
)

(define r run-all)

