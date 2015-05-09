;Andrew Davidson
;Assignment 8

;#1
(define slist-reverse (lambda (slist) (cond [(not (list? slist)) slist][(null? slist) '()][else (reverse (append (map slist-reverse slist)))  ])  ))

(define slist-paren-count (lambda (slist)  (cond [(not (list? slist)) 0][(null? slist) 2][else (+ 2 (apply + (map slist-paren-count slist)))])  ))

(define slist-depth (lambda (slist)  (cond [(not (list? slist)) 0][(null? slist) 1][else (+ 1 (apply max (map slist-depth slist)))])  ))

(define slist-symbols-at-depth 
	(lambda (slist d)   
		(cond 
		[(equal? 1 d) (cond
			[(not (list? slist)) (list slist)]
			[(null? slist) '()]
			[else (apply append 
				(map (lambda (e) 
					(cond 
						[(list? e) '()]
						[else (list e)]) ) 
					slist
				))
			])
		]
		[else (apply append (map (lambda (e)    (cond [(list? e) (slist-symbols-at-depth e (- d 1))][else '()])  )  slist ))])

	)
)

(define slist-map (lambda (proc slist) (cond 
	[(not (list? slist)) (proc slist)]
	[(null? slist) '()]
	[else  (map (lambda (e) (slist-map proc e) ) slist)
	]))
)



;(cond [(not (list? slist)) 0][(null? slist) 1][else (+ 1 (apply max (map slist-depth slist)))])

;#2