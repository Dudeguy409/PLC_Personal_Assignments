;Andrew Davidson
;PLC Exam 1

;#1
(define group-by-two (lambda (ls) (cond 
	[(null? ls) '()]
	[(null? (cdr ls)) (list ls)]
	[else (cons (list (car ls) (cadr ls)) (group-by-two (cddr ls)))]))
)


;#2

;#3

(define ms-contains? (lambda (ms sym)  
	(ormap 
		(lambda (e)  
			(equal? (car e) sym)) 
		ms	 
	))
)

(define ms-add (lambda (ms sym count) (cond [(ms-contains? ms sym) (map  (lambda (e) (if (equal? (car e) sym)  (list (car e) (+ count (cadr e))) e)) ms)][else (append ms (list(list sym count))  )]) ))
(define ms-add-one (lambda (ms sym)  (ms-add ms sym 1)  ))

;#4
(define ms-sym-count 
	(lambda (ms sym)  
		(apply 
			+ 
			(map 
				(lambda (e) (if (equal? sym (car e)) (cadr e) 0 ))
				ms
			) 
		)
	)
)

(define ms-diff (lambda (first second) (apply append (map (lambda (e) (let ([count (ms-sym-count second (car e))])  (cond [(>= count (cadr e)) '()][else (list (list (car e) (-  (cadr e) count ) ) ) ])  ))    first) ) ))

;#5
(define ms-most-frequent (lambda (ms) (if (null? ms) #f (car (ms-most-frequent-help ms)) ) ) )
(define ms-most-frequent-help (lambda (ms) (cond [(null? ms) (list 0 -1)][else (let ([rslt (ms-most-frequent-help (cdr ms))])  (cond [(>= (cadr (car ms)) (cadr rslt)) (car ms)][else rslt]) )]) ))


;#6
(define even-algorithm (lambda (n) (/ n 2)))
(define odd-algorithm (lambda (n) (+ (* 3 n) 1)))
(define hailstone-ls (lambda (n) (hailstone-ls-help n '()) ))
(define hailstone-ls-help (lambda (n ls) (cond 
	[(equal? 1 n) ls]
	[else (cond 
		[(even? n) (let ([rslt (even-algorithm n)])  (hailstone-ls-help rslt (append ls (list rslt))) )]
		[else (let ([rslt (odd-algorithm n)])  (hailstone-ls-help rslt (append ls (list rslt))) )])]) ))
(define hailstone-max-height (lambda (n) (apply max (hailstone-ls n))))

;#7
;copied from assignment 2, adjusted to be inclusive.
(define range (lambda (m n) (if (> m n) '()   (cons m (range (+ m 1) n )))))
(define hailstone-range-max-height (lambda (low high) (apply max  (map  hailstone-max-height (range low high) )  ) ))

;#8
;returns list of hailstone-max-height for all in the given range
(define hailstone-most-frequent-max-help (lambda (low high)   (map  hailstone-max-height (range low high) )   ))


(define ls-unique-elements-help (lambda (ls rslt) (cond 
	[(null? ls) rslt]
	[(ls-contains? (cdr ls) (car ls)) (ls-unique-elements-help (cdr ls) rslt)]
	[else (ls-unique-elements-help (cdr ls) (append rslt (list (car ls) )))]) ))
(define ls-contains? (lambda (ls sym) (cond [(null? ls) #f][(equal? sym (car ls)) #t][else (ls-contains? (cdr ls) sym)]) ) )

;gets a list of all values that appear, then goes back and gets their counts out of the set and creates a formal multiset out of it.  Then, it finds the most common element (see problem 5)
(define hailstone-most-frequent-max (lambda (low high) (let ([ls-of-maxes (hailstone-most-frequent-max-help low high) ])  
	(ms-most-frequent 
		(map  (lambda (e) (list e (ls-sym-count ls-of-maxes e))) (ls-unique-elements-help ls-of-maxes '() )  ) )   )))



(define ls-sym-count (lambda (ls sym)  (cond [(null? ls) 0][(equal? (car ls) sym ) (+ 1 (ls-sym-count (cdr ls) sym))][else (ls-sym-count (cdr ls) sym)]) ))