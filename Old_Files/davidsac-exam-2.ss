(define vector-list
	(lambda (v)
		(let ([vec v][size (vector-length v)][capacity (vector-length v)])

			(lambda (msg  . args)
				(case msg
					[(size) size]
					[(get)   
							(cond 
								[(or (> (car args) (+ size 1) ) (< (car args) 0) )   
									(errorf 'vector-list "invalid index for vector-list object: ~a" msg)]
								[else (vector-ref vec (car args))]
							)
					]
					[(set) (vector-set! vec (car args) (cadr args) ) ]
					[(add)   (cond 
						[(equal? size capacity) (begin (set! vec (double-vec vec capacity) ) (set! capacity (vector-length vec) ) (vector-set! vec size (car args)  ) (set! size (+ size 1) ) (car args)  )]
						[else (begin (vector-set! vec size (car args)  ) (set! size (+ size 1) ) (car args))]) 
					]
					[(remove)    
						(cond 
							[(equal? 0 size)  
								(errorf 'vector-list "illegal message to vector-list object: ~a" msg)
							]
							[(or (> (car args) (+ size 1) ) (< (car args) 0) )   
								(errorf 'vector-list "invalid index for vector-list object: ~a" msg)
							]
							[else 
								(let 
									([elem (vector-ref vec (car args))]) 
									(begin (set! size (- size 1)) (set! vec (vec-remove-elem vec (car args) capacity)) elem)
								)
							]
						)
					]
					[(capacity)    capacity]
					[(getall) vec]
					[else (errorf 'vector-list "illegal message to vector-list object: ~a" msg)]
				)

			)
		)
	)
)

(define vec-remove-elem 
	(lambda 
		(vec index capacity)  
			(let 
				([rslt (make-vector capacity)])
					(vec-remove-elem-help vec rslt 0 index) 
			) 
	)
)

(define vec-remove-elem-help 
	(lambda 
		(old new index eindex)
		(cond 
			[(equal? (vector-length old) (+ index 1)) new]
			[(>= index eindex) (begin (vector-set! new index (vector-ref old (+ 1 index)) ) (vec-remove-elem-help old new (+ 1 index) eindex) )]
			[else 
					 (begin (vector-set! new index (vector-ref old index)) (vec-remove-elem-help old new (+ 1 index) eindex) )	
			]
		) 
				
					
			
	)
)

(define double-vec 
	(lambda (vec capac) 
		(let 
			(
				[new (make-vector  (* 2 capac) )]
			)   

			(double-vec-help vec new 0)
		) 
	)
)


(define double-vec-help 
	(lambda (old new index) 
		(cond [(equal? (vector-length old) index) new][else (begin (vector-set! new index (vector-ref old index) ) (double-vec-help old new (+ index 1) )  )]) 
	)
)