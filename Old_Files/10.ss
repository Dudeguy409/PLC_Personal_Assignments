;Andrew Davidson
;Assigntment #10

;#1

(define make-stack
	(lambda ()
		(let ([stk '()])
			(lambda (msg  . args ) 
				(case msg
					[(empty?) (null? stk)]
					[(push)   (set! stk (cons (car args) stk))]
					[(pop)    (let ([top (car stk)])(set! stk (cdr stk))top)]
					[else (errorf 'stack "illegal message to stack object: ~a" msg)]
				)
			)
		)
	)
)

(define make-slist-leaf-iterator
	(lambda (args)
		(let
			(
				[ls args]
				[stk (make-stack)]
			)
			(letrec
				(
					[iter
						(lambda ()
							(cond
								[(not(stk 'empty?))
									(let
										([elem (stk 'pop)])
										(cond 
											[(symbol? elem)
												elem
											]
											[(null? elem)
												(iter)
											]
											[else
												(begin
													(stk 'push (cdr elem))
													(stk 'push (car elem))
													(iter)
												)
											]
										)
									)
								]
								[(null? ls) 
									#f
								]
								[else 
									(begin 
										(stk 'push (car ls))  
										(set! ls (cdr ls)) 
										(iter)
									) 
								]
							)
						)
					]
				)
				iter
			)
		)
	)
)


;#2
(define free-vars
	(lambda 
		(exp)
		(cond
			[(symbol? exp) 
				(list exp)
			]
			[(eqv? (car exp) 'lambda)
				(let ([ls (free-vars (caddr exp))])  (remove-first (caadr exp) ls) )
			]
			[else 
				(remove-duplicates (append (free-vars  (car exp))
					(free-vars (cadr exp))
				))
			]
		)
	)
)

(define remove-first 
	(lambda 
		(element ls) 
		(cond 
			[(null? ls) 
				ls
			]
			[(equal? element (car ls)) 
				(cdr ls)
			]
			[else 
				(append 
					(list (car ls))   
					(remove-first element (cdr ls)) 
				)
			]
		)
	)
)

(define contains? 
	(lambda 
		(e s2) 
		(if 
			(null? s2) 
				#f 
				(if 
					(equal? e (car s2)) 
						#t 
						(contains? e (cdr s2))
				)
		)
	)
)

(define remove-duplicates 
	(lambda (ls) 
		(cond
			[(null? ls) ls] 
			[(contains? (car ls) (cdr ls)) (remove-duplicates (cdr ls))]
			[else
				(cons
					(car ls) 
					(remove-duplicates (cdr ls))
				)
			] 		
		)
	)
)

(define bound-vars
	(lambda (exp)
		(cond
			[(symbol? exp)
				'()
			]
			[(eqv? (car exp) 'lambda)
				(let 
					(
						[e (caadr exp)]
						[ls (free-vars (caddr exp))]
					)
					(cond 
						[(contains? e ls) (cons e (bound-vars (caddr exp)) )]
						[else (bound-vars (caddr exp))]
					)
				)
			]
			[else 
				(append 
					(bound-vars  (car exp))
					(bound-vars (cadr exp))
				)
			]
		)
	)
)


;#3
(define occurs-free?
	(lambda 
		(var exp)
		(cond
			[(symbol? exp) 
				(eqv? var exp)
			]
			[(eqv? (car exp) 'lambda) 
				(and (not (eqv? (caadr exp) var))
					(occurs-free? var (caddr exp))
				)
			]
			[else 
				(or (occurs-free? var  (car exp))
					(occurs-free? var (cadr exp))
				)
			]
		)
	)
)

(define occurs-bound?
	(lambda (var exp)
		(cond
			[(symbol? exp) 
				#f
			]
			[(eqv? (car exp) 'lambda)
				(or 
					(occurs-bound? var (caddr exp))
					(and 
						(eqv? (caadr exp) var)
						(occurs-free? var (caddr exp))
					)
				)
			]
			[else 
				(or 
					(occurs-bound? var  (car exp))
					(occurs-bound? var (cadr exp))
				)
			]
		)
	)
)

;#4

(define lexical-address 
	(lambda (exp)
		(lexical-address-help exp -1 '()) 
	)
)

(define lexical-address-help
	(lambda (exp depth ls)
		(cond
			[(symbol? exp)
			 (let ([rslt (if (bound-ls-contains? exp ls) (bound-ls-get-first exp ls depth) (list ': 'free exp) )]) rslt )
			]
			[(null? exp) '()]
			[(eqv? (car exp) 'lambda)

			(let ([bound-ls (append (get-address-tuples (+ 1 depth) (cadr exp) ) ls)]) (append (cons (car exp)(list (cadr exp))) (list (lexical-address-help (caddr exp) (+ 1 depth)  bound-ls )))  )
			]
			[(equal? (car exp) 'if) (cons (car exp) (lexical-address-help (cdr exp) depth ls))]
			[(symbol? (car exp)) 

			(let ([rslt (if (bound-ls-contains? (car exp) ls) (bound-ls-get-first (car exp) ls depth) (list ': 'free (car exp)) )]) (append (list rslt) (lexical-address-help (cdr exp) depth ls)) )

			]



			[(list? (car exp)) (append (list (lexical-address-help (car exp) depth ls))   (lexical-address-help (cdr exp) depth ls) )]
			[else
			;(syntax? (car exp)) 
				(cons (car exp) (lexical-address-help (cdr exp) depth ls))
			]
		)
	)
)

(define get-address-tuples 
	(lambda 
		(depth params)  
		(get-address-tuples-help depth params 0)  
	)
)

(define get-address-tuples-help 
	(lambda 
		(depth params index)  
		(cond 
			[(null? params) 
				'()
			]
			[else 
				(cons 
					(list (car params) depth index)  
					(get-address-tuples-help depth (cdr params) (+ 1 index) ) 
				)
			]
		)  
	)
)

(define bound-ls-contains? 
	(lambda 
		(sym ls) 
		(cond 
			[(null? ls) #f]
			[(equal? sym (caar ls)) #t]
			[else (bound-ls-contains? sym (cdr ls))]
		) 
	)
)

(define bound-ls-get-first (lambda (sym ls depth) (cond [(null? ls) #f][(equal? sym (caar ls)) (list ': (- depth (cadr(car ls))) (caddr(car ls)))][else (bound-ls-get-first sym (cdr ls) depth)]) ))
  



  ;#5

  (define un-lexical-address 
	(lambda (exp)
		(un-lexical-address-help exp -1 '()) 
	)
)

  (define un-lexical-address-help
	(lambda (exp depth ls)
		(cond
			[(null? exp) '()]
			[(eqv? (car exp) 'lambda)
			(let ([bound-ls (append (get-address-tuples (+ 1 depth) (cadr exp) ) ls)]) (append (cons (car exp)(list (cadr exp))) (list (un-lexical-address-help (caddr exp) (+ 1 depth)  bound-ls )))  )
			]
			[(and 
				(list? exp) 
				(not (null? (cdr exp))) 
				(equal? ': (car exp))  
			 ) 
				(cond 
					[(equal? 'free (cadr exp)) (caddr exp)]
					[else (bound-ls-get-var ls  (- depth (cadr exp)) (caddr exp) )]
				)
			]
			[(list? (car exp)) (append (list (un-lexical-address-help (car exp) depth ls))   (un-lexical-address-help (cdr exp) depth ls) )]
			[else
				(cons (car exp) (un-lexical-address-help (cdr exp) depth ls))]


				)



	)
)

(define bound-ls-get-var 
	(lambda 
		(ls depth pos) 
		(cond 
			[(null? ls) #f]
			[(and 
				(equal? depth (cadr (car ls)) ) 
				(equal?  (caddr (car ls))  pos)) 

				(car (car ls))
			]
			[(bound-ls-get-var (cdr ls) 
				depth pos)]
		) 
	)
)