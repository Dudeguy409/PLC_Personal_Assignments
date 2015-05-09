;Andrew Davidson

;Assignment 15

(define apply-continuation 
	(lambda 
		(k . list-of-values)  
			(apply k list-of-values)  
	)  
)

;#1

(define member?-cps
	(lambda
		(e ls k)
			(cond
				[(null?  ls) (apply-continuation k #f)]
				[(equal? e  (car ls))  (apply-continuation k #t)]
				[else (member?-cps e (cdr ls) k)]
			)
	)
)

;#2

(define set?-cps
	(lambda 
		(ls k)
			(cond
				[(null? ls) (apply-continuation k #t)]
				[(not (pair? ls)) (apply-continuation k #f)]
				[else 
					(member?-cps
						(car ls) 
						(cdr ls) 
						(lambda
							(rslt)  
								(if 
									rslt 
										(apply-continuation k #f) 
										(set?-cps (cdr ls) k)
								)   
						)  
					) 
				]
			)
	)
)

;#3

; removes duplicates to make a set
(define set-of-cps
	(lambda
		(ls k)
			(cond
				[(null? ls)
					(apply-continuation k '())
				]
				[else
					(member?-cps
						(car ls)
						(cdr ls)
						(lambda
							(rslt)
								(if
									rslt
										(set-of-cps (cdr ls) k)
										(set-of-cps 
											(cdr ls) 
											(lambda 
												(x)
													(apply-continuation k (cons (car ls) x) )  
											)
										)
								)
						)
					)
				]
			)
	)
)

; finds the domain of a relation.
(define domain-cps
	(lambda 
		(rel k)
			(map-cps 
				1st-cps 
				rel 
				(lambda 
					(ls) 
						(set-of-cps ls k) 
				) 
			)
	)
)

; any procedure that map-cps takes as its first argument must be in CPS form.
(define map-cps
	(lambda
		(proc-cps ls k)
			(cond
				[(null? ls)
					(apply-continuation k '())
				]
				[else
					(proc-cps  
						(car ls)  
						(lambda 
							(rslt) 
								(map-cps proc-cps  
									(cdr ls)  
									(lambda 
										(x)  
											(apply-continuation 
												k  
												(cons rslt x) 
											) 
									)  
								)  
						) 
					)
				]
			)  
	)
)

(define andmap-cps
	(lambda
		(proc-cps ls k)
			(cond
				[(null? ls)
					(apply-continuation k #t)
				]
				[else
					(proc-cps
						(car ls)
						(lambda
							(rslt)
								(if
									rslt
										(andmap-cps proc-cps
											(cdr ls)
											k
										)
										(apply-continuation k #f)
								)
						)
					)
				]
			)
	)
)

(define make-cps 
	(lambda 
		(proc) 
			(lambda 
				(x k)  
					(apply-continuation k (proc x))
			)  
	) 
)


(define 1st-cps
	(lambda 
		(ls k)  
			(apply-continuation k (car ls)) 
	)
)

;#4

(define +-cps 
	(lambda 
		(a b k) 
			(apply-continuation k (+ a b))
	)
)

(define reverse-cps
	(lambda
		(ls k)
			(reverse-cps-help ls '() k)
	)
)

(define reverse-cps-help
	(lambda
		(ls rslt k)
			(if
				(null?  ls)
					(apply-continuation k rslt)
					(reverse-cps-help (cdr ls) (cons (car ls) rslt) k)
			)
	)
)

(define apply-cps 
	(lambda 
		(proc ls k) 
			(apply-continuation k  (apply proc ls) )  
	) 
)

(define snlist-recur-cps
	(lambda 
		(base ls-proc-cps e-proc-cps) 
			(letrec 
				(
					[helper   
						(lambda
							(snls k)  
								(if 
									(not (list? snls) ) 
										(e-proc-cps snls k) 
										(if 
											(null? snls) 
												(apply-continuation k base) 
												(map-cps (lambda (e kont) (helper e kont)) snls  (lambda (x) (ls-proc-cps x k) ) )  
										) 
								) 
								   
						)   
					]
				) 
				helper  
			)  
	)
)


(define sn-list-reverse-cps 
	(snlist-recur-cps
		'()
		reverse-cps
		(lambda
			(e k)
				(apply-continuation k e)
		)
	)
)

(define sn-list-sum-cps 
	(snlist-recur-cps
		0
		(lambda
			(e k)
				(apply-cps + e k)
		)
		(lambda
			(e k)
				(apply-continuation k e)
		)
	)
)

(define sn-list-occur-cps
	(lambda
		(s ls k)
			(
				(snlist-recur-cps
					0
					(lambda
						(e kont)
							(apply-cps + e kont)
					)
					(lambda
						(e kont)
							(if
								(equal? e s)
									(apply-continuation kont 1)
									(apply-continuation kont 0)
							)
					)
				)
				ls k
			)
	)
)

(define sn-list-depth-cps
	(snlist-recur-cps
		1
		(lambda
			(e k)
				(apply-cps
					max
					e
					(lambda
						(x)
							(+-cps
								x
								1
								k
							)
					)
				)
		)
		(lambda
			(e k)
				(apply-continuation k 0)
		)
	)
)

;2

(define memoize
	(lambda
		(f hash equiv?)
				(let
					(
						[t (make-hashtable hash equiv?)]
					)
									(lambda
										y
											(if
												(hashtable-contains? t y )
													(hashtable-ref t  y 0)
													(begin
														(hashtable-set! t y (apply f y))
														(hashtable-ref t y 0)
													)
											)
									)
									
								
				)
			
	)
)