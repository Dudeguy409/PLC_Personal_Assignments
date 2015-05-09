;Andrew Davidson
;Assignment 7

;#1
;get length of list, create new vector, set all old values, add elems of list to new vector
(define vector-append-list 
  (lambda (vec ls)
	(vector-append-list-help-copy-list
	  (vector-length vec)
	  (vector-append-list-help-copy-old 
		0
		(vector-length vec)
		(make-vector (+ (vector-length vec) (length ls)))
		vec)
	  ls)))

(define vector-append-list-help-copy-old 
  (lambda 
    (start end vec old-vec) 
	(cond 
	  [(equal? start end) vec]
      [else 
		(vector-append-list-help-copy-old 
		  (begin 
		    (vector-set! vec start (vector-ref old-vec start)) 
			(+ start 1)
		  ) 
		  end
		  vec 
		  old-vec
		)
	  ]
	)
  )
)

(define vector-append-list-help-copy-list 
  (lambda 
    (index vec ls)  
	(cond 
	  [(null? ls) vec]
	  [else 
	    (vector-append-list-help-copy-list 
		  (begin 
		    (vector-set! vec index (car ls)) 
		    (+ index 1)
		  ) 
		  vec 
		  (cdr ls)
		)
	  ]
	)
  )
) 

;#2
(define filter-out (lambda (pred? ls)  (apply append (map (lambda (e) (if (pred? e) '() (list e) )) ls))))
(define qsort (lambda (pred ls)  (cond 
	[(null? ls) ls]
	[(null? (cdr ls)) ls]
	[else 
		(let (
			[pre-ls (filter-out (lambda (e) (pred (car ls) e)) (cdr ls))]
			[post-ls (filter-out (lambda (e) (not(pred (car ls) e))) (cdr ls))])      
				(append 
					(qsort pred pre-ls) 
					(list (car ls)) 
					(qsort pred post-ls) 
				)       
		)
	]))
)

;#3

;#4
;(define reverse-it (lambda (ls) (reverse ls)))
(define reverse-it (lambda (ls) (cond 
	[(null? ls) ls]
	[else (append (reverse-it (cdr ls))  (list (car ls))  )]))
)


;#5
(define empty-BST 
	(lambda ()   '()  )
)

(define empty-BST? (lambda (bst) (and (null? bst) (list? bst))))


(define BST-help-syntax? 
	(lambda (bst) 
		(cond 
			[(not (list? bst )) #f]
			[(null? bst) #t]
			[else 
				(and 
					(equal? 3 (length bst)) 
					(number? (car bst)) 
					(BST-help-syntax? (cadr bst)) 
					(BST-help-syntax? (caddr bst)) 
				)
			]
		)
	)
)

(define BST-help-valid-order 
	(lambda (bst)   
		(cond 
			[(null? bst) (list #t)]
			[	
				(and 
					(null? (caddr bst))  
					(null? (cadr bst))) 
				(list #t (car bst) (car bst)
				)
			]
			[(null? (caddr bst))  
				(let ([x (BST-help-valid-order (cadr bst))]) 
					(cond 
						[(not (car x)) (list #f)]
						[(<= (car bst) (caddr x)) (list #f)]
						[else (list #t (cadr x) (car bst))]
					)
				) 
			]
			[(null? (cadr bst))  
				(let ([y (BST-help-valid-order (caddr bst))]) 
					(cond 
						[(not (car y)) (list #f)]
						[(>= (car bst) (cadr y)) (list #f)]
						[else (list #t  (car bst) (caddr y))]
					)
				)
			]
			[else 
				(let 
					([x (BST-help-valid-order (cadr bst))]
					[y (BST-help-valid-order (caddr bst))]) 
						(cond 
							[(not (car x)) (list #f)]
							[(not (car y)) (list #f)]
							[(<= (car bst) (caddr x)) (list #f)]
							[(>= (car bst) (cadr y)) (list #f)]
							[else (list #t  (cadr x) (caddr y))]
						)
				)
			]
		)  
	)
)




(define BST? (lambda (bst) (and (BST-help-syntax? bst) (car (BST-help-valid-order bst)))))


(define BST-inorder (lambda (bst) (cond [(null? bst) '()][else (append (BST-inorder (cadr bst)) (list (car bst)) (BST-inorder (caddr bst)))])))

(define BST-contains? (lambda (bst num) (cond [(null? bst) #f][(equal? num (car bst))][(< num (car bst)) (BST-contains? (cadr bst) num)][(> num (car bst)) (BST-contains? (caddr bst) num)])))

(define BST-insert-nodes (lambda (bst nums) (cond [(null? nums) bst][else (BST-insert-nodes (BST-insert (car nums) bst) (cdr nums) )])))
(define BST-element (lambda (bst) (car bst)))
(define BST-left (lambda (bst) (cadr bst)))
(define BST-right (lambda (bst) (caddr bst)))

(define BST-insert (lambda (num bst) (cond 
	[(null? bst) (list num '() '())]
	[(equal? (car bst) num) bst]
	[(> num (car bst)) (list (car bst) (cadr bst) (BST-insert num (caddr bst)) ) ]
	[else  (list (car bst) (BST-insert num (cadr bst)) (caddr bst) )])))




;#6
(define map-by-position (lambda (fn-list arg-list)  (map 
	(lambda (ls)   ((car ls) (cadr ls))) (matrix-transpose (list fn-list arg-list))

	)))


;(define map-by-position (lambda (fn-list arg-list) (matrix-transpose (list fn-list arg-list))))


(define get-cdrs 
	(lambda (l) 
		(map 
			(lambda (e) (cdr e))  
			l 
		)
	)
)
(define get-cars 
	(lambda (l) 
		(map 
			(lambda (e) (car e))  
			l 
		)
	)
)

(define matrix-transpose 
	(lambda (l)     
		(matrix-transpose-help l '())
	)
)

(define matrix-transpose-help 
	(lambda (l rslt)     
		(if 
			(null? (car l)) 
				rslt 
				(matrix-transpose-help 
					(get-cdrs l) 
					(append rslt (list (get-cars l)))
				)     
		)   
	)
)






;#7
(define bt-leaf-sum (lambda (T)  (cond [(number?  T)  T][else (+ (bt-leaf-sum (cadr T)) (bt-leaf-sum (caddr T)) )])   ))
(define bt-inorder-list (lambda (T)  (cond [(number? T) '()][else (append  (bt-inorder-list (cadr T))  (list (car T)) (bt-inorder-list (caddr T)) )]) ))
(define bt-max (lambda (T)        (cond [(number? T) T][else (max (bt-max (cadr T))  (bt-max (caddr T))   )])     ))


(define bt-max-interior (lambda (T) (car (bt-max-interior-help T))   ))
(define bt-max-interior-help (lambda (T) (cond 
	[(and (number? (cadr T)) (number? (caddr T)) )  (list (car T)  (+ (cadr T) (caddr T) ) (+ (cadr T) (caddr T) ) ) ]
	[(number? (cadr T))  (let ([rslt (bt-max-interior-help (caddr T))])     (let ([total (+ (cadr T) (caddr rslt))])    (cond 
		[(and (> total (cadr T))  (>= total (cadr rslt)) ) (list (car T) total total)]
		[else (list (car rslt)(cadr rslt) total)])  )              ) ]
	[(number? (caddr T))  (let ([rslt (bt-max-interior-help (cadr T))])     (let ([total (+ (caddr T) (caddr rslt))])    (cond 
		[(and (>= total (caddr T))  (> total (cadr rslt)) ) (list (car T) total total)]
		[else (list (car rslt)(cadr rslt) total)])  )              ) ]
	[else (let ([rt-rslt (bt-max-interior-help (caddr T))][lt-rslt (bt-max-interior-help (cadr T))])     
		(let ([total (+ (caddr lt-rslt) (caddr rt-rslt))])     (cond 
		[(and (> total (cadr lt-rslt))  (>= total (cadr rt-rslt)) ) (list (car T) total total)]
		[else  (cond [(>= (cadr lt-rslt) (cadr rt-rslt) ) (list (car lt-rslt)(cadr lt-rslt) total)][else (list (car rt-rslt)(cadr rt-rslt) total)])])                          )    )]
	)))

