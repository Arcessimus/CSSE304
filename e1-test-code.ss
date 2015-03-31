; Test code for CSSE 304 Assignment 7

(define (test-group-by-two)
    (let ([correct '(
		     ()
		     ((a))
		     ((a b))
		     ((a b) (c))
		     ((a b) (c d) (e f) (g))
		     ((a b) (c d) (e f) (g h))
		     )]
          [answers 
	   (list 
	    (group-by-two '())
	    (group-by-two '(a))
	    (group-by-two '(a b))
	    (group-by-two '(a b c))
	    (group-by-two '(a b c d e f g))
	    (group-by-two '(a b c d e f g h))
 )])
      (display-results correct answers equal?)))

(define (test-group-by-n)
    (let ([correct '(
		    ()
		    ((a b c) (d e f) (g))
		    ((a b c d) (e f g))
		    ((a b c d) (e f g h))
		    ((a b c d e f g) (h i j k l m n) (o))
		    ((a b c d e f g h))
		    ((a b c d e f g h i j k l m n o p q) (r s t))
		    )]
          [answers 
            (list 
	     (group-by-n '() 3)
	     (group-by-n '(a b c d e f g) 3)
	     (group-by-n '(a b c d e f g) 4)
	     (group-by-n '(a b c d e f g h) 4)
	     (group-by-n '(a b c d e f g h i j k l m n o) 7)
	     (group-by-n '(a b c d e f g h) 17)
	     (group-by-n '(a b c d e f g h i j k l m n o p q r s t) 17)
	     )])
      (display-results correct answers equal?)))

(define (test-ms-add-one)
    (let ([correct '(
		    ((a 4) (b 4))
		    ((a 3) (b 5))
		    ((a 3) (b 4) (c 1))
		    )]
          [answers 
            (list 
	     (ms-add-one '((a 3) (b 4)) 'a)
	     (ms-add-one '((a 3) (b 4)) 'b)
	     (ms-add-one '((a 3) (b 4)) 'c)
	     )])
      (display-results correct answers sequal?-grading)))

(define (test-ms-diff)
    (let ([correct '(
		     ()
		     ()
		     ((a 1))
		     ()
		     ((b 1) (a 1))
		     ((b 1))
		     ((b 1) (a 1))
		     ((c 2))
		     )]
          [answers 
            (list 
	     (ms-diff '() '())
	     (ms-diff '() '((a 1)))
	     (ms-diff '((a 2)) '((a 1)))
	     (ms-diff '((a 2)) '((a 2)))
	     (ms-diff '((a 2)(b 6)) '((b 5) (a 1)))
	     (ms-diff '((a 2)(b 6)) '((a 2) (b 5)))
	     (ms-diff '((b 5) (a 2)) '((a 1) (b 4) (c 2)))
	     (ms-diff '((b 3)(a 2)(c 2)) '((a 2) (b 4)))
	     )])
      (display-results correct answers sequal?-grading)))

(define (test-ms-most-frequent)
    (let ([correct '(
		     #f
		     b
		     c
		     d
		     e
		     f
		     )]
          [answers 
            (list 
	     (ms-most-frequent '())
	     (ms-most-frequent '((a 3) (b 4) (c 2) (d 3)))
	     (ms-most-frequent '((c 4) (h 2) (d 3) (e 4)))
	     (ms-most-frequent '((a 3) (d 4)))
	     (ms-most-frequent '((e 4)))
	     (ms-most-frequent '((a 3) (f 4) (c 2) (d 3) (e 4)))
	     )])
      (display-results correct answers equal?)))

(define (test-hailstone-max-height)
    (let ([correct '(
		     16
		     16
		     52
		     160
		     88
		     9232
		     )]
          [answers 
            (list 
	     (hailstone-max-height 3)
	     (hailstone-max-height 5)
	     (hailstone-max-height 7)
	     (hailstone-max-height 15)
	     (hailstone-max-height 19)
	     (hailstone-max-height 27)
	     )])
      (display-results correct answers equal?)))

(define (test-hailstone-range-max-height )
    (let ([correct '(
		     52
		     9232
		     13120
		     )]
          [answers 
            (list 
	     (hailstone-range-max-height 7 14)
	     (hailstone-range-max-height 10 50)
	     (hailstone-range-max-height 200 300)
	     )])
      (display-results correct answers equal?)))

(define (test-hailstone-most-frequent-max)
    (let ([correct '(
		    16
		    9232
		    88
		    9232
		    190996
		    )]
          [answers 
            (list 
	     (hailstone-most-frequent-max 3 5)
	     (hailstone-most-frequent-max 30 100)
	     (hailstone-most-frequent-max 25 50)
	     (hailstone-most-frequent-max 100 200)
	     (hailstone-most-frequent-max 9232 9350)
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
  (display 'group-by-two) 
  (test-group-by-two)
  (display 'group-by-n) 
  (test-group-by-n)
  (display 'ms-add-one) 
  (test-ms-add-one)
  (display 'ms-diff) 
  (test-ms-diff)    
  (display 'ms-most-frequent) 
  (test-ms-most-frequent)
  (display 'hailstone-max-height) 
  (test-hailstone-max-height)  
  (display 'hailstone-range-max-height ) 
  (test-hailstone-range-max-height )  
  (display 'hailstone-most-frequent-max) 
  (test-hailstone-most-frequent-max)
)

(define r run-all)

