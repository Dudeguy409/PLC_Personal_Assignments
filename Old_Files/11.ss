;Andrew Davidson
;Assignment 11

; starting code for the last two problems.  
; The file being loaded here should live in the same folder as this one.

(load "chez-init.ss")




;#1

;a
(define-syntax my-let 
	(syntax-rules () 
		[(_ ((x v) ...) e1 e2 ...)((lambda (x ...) e1 e2 ...) v ...)]
		[(_ x ([x1 n1] [x2 n2] ...) exp)  (letrec ([x (lambda (x1 x2 ...) exp )]) (x n1 n2 ...) )]
	)
)


;b
(define-syntax my-or 
	(syntax-rules ()  
		[(_ ) #f]
		[(_ a1)  a1 ]
		[(_  a1 a2 ...)     (let ([z a1])  (if z z (my-or a2 ...))  ) ]
	)
)

;c
(define-syntax += 
	(syntax-rules ()  
		[(_ x y)   (begin (set! x (+ x y)) x  )]
	)
)

;d
(define-syntax return-first 
	(syntax-rules ()
		[(_ e1)  e1]  
		[(_ e1 e2 ...) (let* ([rslt e1][b (begin e2 ...)]) rslt )]
	)
)

;2
(define-datatype 
	bintree
	bintree?
		[leaf-node
			(datum number?)]
		[interior-node 
			(key symbol?)
			(left-tree bintree?)
			(right-tree bintree?)
		]
)


(define 
	actual-bintree-to-list
	(lambda 
		(tree)
		(cases bintree tree 
			[leaf-node (num) num]
			[interior-node (sym lt rt) (list sym (actual-bintree-to-list lt)(actual-bintree-to-list rt))]
		)
	)
)


(define
	bintree-to-list
	(lambda
		(tree)
		(cases bintree tree 
			[leaf-node (num) (list 'leaf-node num)]
			[interior-node (sym lt rt) (list  'interior-node sym   (bintree-to-list lt)(bintree-to-list rt)  )]
		)
	)
)






;3
(define max-interior (lambda (T) (car(bt-max-interior-help T))   ))
(define bt-max-interior-help 
	(lambda
		(tree)
		(cases bintree tree
			[leaf-node (num) 
				(eopl:error 'bt-max-interior-help "a leaf-node has been accessed in max-interior" num)]
			[interior-node (sym lt rt) 
				(cond 
					[(and (leaf-node?  lt) (leaf-node?  rt) )
						(let ([sum (+ (get-leaf-int lt) (get-leaf-int rt))]) (list sym sum sum)   )
					]
					[(leaf-node?  lt)
						(let
							([rslt (bt-max-interior-help rt)])
								(let ([total (+ (get-leaf-int lt) (caddr rslt))])
									(cond 
										[(>= total (cadr rslt))  
											(list sym total total)
										]
										[else 
											(list (car rslt) (cadr rslt) total)
										]
									)  
								)              
						)
					]
					[(leaf-node?  rt) 
						(let ([rslt (bt-max-interior-help lt)])     
							(let ([total (+ (get-leaf-int rt) (caddr rslt))])    
								(cond 
									[(> total (cadr rslt)) 
										 (list sym total total)
									]
									[else (list (car rslt) (cadr rslt) total)]
								)  
							)              
						) 
					]
					[else 
						(let 
							(
								[rt-rslt (bt-max-interior-help rt)]
								[lt-rslt (bt-max-interior-help lt)]
							)     
							(let ([total (+ (caddr lt-rslt) (caddr rt-rslt))])     
								(cond
									[(and (> total (cadr lt-rslt))  (>= total (cadr rt-rslt)) )
										(list sym total total)
									]
									[else  
										(cond 
											[(>= (cadr lt-rslt) (cadr rt-rslt) ) 
										(list (car lt-rslt)(cadr lt-rslt) total)
											]
											[else 
												(list (car rt-rslt)(cadr rt-rslt) total)
											]
										)
									]
								)                          
							)    
						)
					]
				)
			]
		)
	)
)

(define leaf-node?  (lambda (tree)  (cases bintree tree [leaf-node (num) #t][interior-node (sym lt rt) #f]) ))
(define get-leaf-int  (lambda (tree)  (cases bintree tree [leaf-node (num) num][interior-node (sym lt rt) (eopl:error 'bt-max-interior-help "tried to get leaf-value of an interior node" tree)]) ))


;4 -------------------------------------------------------------------------------------------------------------------------------------------------------------------------

(define-datatype 
	expression  
	expression? 
		[lit-exp (id (lambda (x) (or (vector? x) (number? x) ) ))]
		[var-exp (id symbol?)]
		[set-bang-exp  (id symbol?)  (exp expression?) ]
		[lambda-exp (id (list-of expression?)) (body (list-of expression?))]
		[lambda-var-exp (id expression?) (body (list-of expression?))]
		[app-exp (rator expression?) (rand (list-of expression?))]
		[named-let-exp (name expression?) (id (list-of tuple?)) (body (list-of expression?)) ]
		[let-exp  (id (list-of tuple?)) (body (list-of expression?)) ]
		[let-star-exp  (id (list-of tuple?)) (body (list-of expression?)) ]
		[letrec-exp  (id (list-of tuple?)) (body (list-of expression?)) ]
		[if-exp (exp1 expression?) (exp2 expression?) ]
		[if-else-exp (exp1 expression?) (exp2 expression?)  (exp3 expression?) ]
)

(define-datatype 
	tuple  
	tuple? 
		[var-exp-tuple (id symbol?)(exp expression?)]
)

(define parse-exp 
	(lambda 
		(datum)
		(cond 
			[(number? datum) (lit-exp datum)]
			[(vector? datum) (lit-exp datum)]
			[(symbol? datum) (var-exp datum)]
			[(list? datum)
				(cond 
					[(eqv?  (car datum) 'if)  
						(cond 
							[(null? (cdr datum)) (eopl:error 'parse-exp "There are no expressions in the if statement" datum)]
							[(null? (cddr datum)) (eopl:error 'parse-exp "There is no then-expression in the if statement" datum)]
							[(null? (cdddr datum)) (if-exp (parse-exp (cadr datum)) (parse-exp (caddr datum))) ]
							[(null?  (cddddr datum) )  (if-else-exp (parse-exp (cadr datum)) (parse-exp (caddr datum))  (parse-exp (cadddr datum)) ) ]
							[else (eopl:error 'parse-exp "There are too many expressions in the if statement" datum)]
						)
						
					]
					[(eqv? (car datum) 'lambda)
						(let  ([body  (if (null? (cddr datum)) (eopl:error 'parse-exp "no body contained in the lambda expression" datum) (map parse-exp (cddr datum)) ) ]) 
							(cond [(symbol? (cadr datum)) (lambda-var-exp (parse-exp (cadr datum)) body)  ][else (lambda-exp (map check-params (cadr datum) ) body)  ]  )
						 )
					]
					[(eqv? (car datum) 'let*)
						(let-star-exp
							(if (not (list? (cadr datum))) (eopl:error 'parse-exp "list of tuples in the let* expression is not a list" datum) (map parse-tuple (cadr datum)))
							(if (null? (cddr datum)) (eopl:error 'parse-exp "no body contained in the let* expression" datum) (map parse-exp (cddr datum)) )
						)
					]
					[(eqv? (car datum) 'set!)
						(if (not (equal? 3  (length datum)) ) (eopl:error 'parse-exp "set! has the wrong number of parameters" datum) 
							(set-bang-exp
								(if (symbol? (cadr datum) ) (parse-exp (cadr datum))  (eopl:error 'parse-exp "the variable to be set in set! is not actually a variable" datum) )
								(parse-exp (cddr datum))
							)
						)
					]
					[(eqv? (car datum) 'letrec)
						(letrec-exp
							(if (not (list? (cadr datum))) (eopl:error 'parse-exp "list of tuples in the letrec expression is not a list" datum) (map parse-tuple (cadr datum)))
							(if (null? (cddr datum)) (eopl:error 'parse-exp "no body contained in the letrec expression" datum) (map parse-exp (cddr datum)) )
						)
					]
					[(eqv? (car datum) 'let)
						(cond 
							[(not (list? (cadr datum) ))  
							(let 
								([name (parse-exp (cadr datum))]) 
								(cases expression name 
									[var-exp (id)  
										(named-let-exp
											name
											(if (not (list? (cadr datum))) (eopl:error 'parse-exp "list of tuples in the named let expression is not a list" datum) (map parse-tuple (cadr datum)))
											(if (null? (cdddr datum)) (eopl:error 'parse-exp "no body contained in the let expression" datum) (map parse-exp (cdddr datum)) )
										)
							 		][else (eopl:error 'parse-exp "the name for the named-let was not a symbol" datum)]
							 	) 
							)
							]
							[else
								(let-exp
									(if (not (list? (cadr datum))) (eopl:error 'parse-exp "list of tuples in the let expression is not a list" datum) (map parse-tuple (cadr datum)))
									(if (null? (cddr datum)) (eopl:error 'parse-exp "no body contained in the let expression" datum) (map parse-exp (cddr datum)) )
								)
							]
						)
					]
					[else (app-exp (parse-exp (car datum))(map parse-exp (cdr datum)))]
				)
			]
			[else (eopl:error 'parse-exp "Invalid concrete syntax ~s" datum)]
		)
	)
)

(define parse-tuple 
	(lambda (node)
		(if 
			(or  (not(list? node)) (not (equal? (length node) 2)) (not (symbol? (car node)) )) 
			(eopl:error 'parse-exp "incorrect tuple structure" node)  
			(var-exp-tuple (car node) (parse-exp (cadr node)))
		)
	)
)

(define check-params 
	(lambda (p) 
		(if (not (symbol? p) )  
			(eopl:error 'parse-exp "parameters must be symbols" p)    
			(parse-exp p) 
		)
	)
)



(define 
	unparse-exp
	(lambda
		(exp)
		(cases expression exp
			[var-exp (id) id]
			[lit-exp (id) id]
			[set-bang-exp (id exp)  (list 'set! (unparse-exp id) (unparse-exp exp)  ) ]
			[if-exp (exp1 exp2)  (list 'if (unparse-exp exp1) (unparse-exp exp2) ) ]
			[if-else-exp (exp1 exp2 exp3)  (list 'if (unparse-exp exp1) (unparse-exp exp2)  (unparse-exp exp3)  )]
			[lambda-exp (id body) (append (list 'lambda (map unparse-exp id) ) (map unparse-exp body))]
			[lambda-var-exp (id body) (append (list 'lambda (unparse-exp id) ) (map unparse-exp body))]
			[named-let-exp (name id body) (append (list 'let (unparse-exp name) (map unparse-tuple id) ) (map unparse-exp body))]
			[let-exp (id body) (append (list 'let (map unparse-tuple id) ) (map unparse-exp body))]
			[let-star-exp (id body) (append (list 'let* (map unparse-tuple id) ) (map unparse-exp body))]
			[letrec-exp (id body) (append (list 'letrec (map unparse-tuple id) ) (map unparse-exp body))]
			[app-exp (rator rand)(append (list (unparse-exp rator)  ) (map unparse-exp rand)) ]
		)
	)
)

(define unparse-tuple (lambda (tuple-exp) (cases tuple tuple-exp  [var-exp-tuple (id exp) (list id (unparse-exp exp))]) ))