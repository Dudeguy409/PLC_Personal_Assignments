;Andrew Davidson
;Assignment 5


;#1
;(define interval-contains? (lambda (m n)  (if (< n (car m)) #f (if (> n (cadr m)) #f #t))))
;(define interval-intersects? (lambda (m n)  (if (interval-contains? m (car n))#t (if (interval-contains? m (cadr n))#t (if (interval-contains? n (car m))#t (if (interval-contains? n (cadr m))#t #f))))))
;(define find-min (lambda (m n) (if(< (car m) (car n)) (car m)  (car n))))
;(define find-max (lambda (m n) (if(> (cadr m) (cadr n)) (cadr m)  (cadr n))))
;(define interval-union (lambda (m n) (if (interval-intersects? m n) (list (list (find-min m n) (find-max m n))) (list m n))))

;(define minimize-interval-list (lambda (ls) (find-interval-unions ls '())))
;(define find-interval-unions (lambda (ls rslt) (cond [(null? ls) rslt][(null? (cdr ls)) rslt][else (find-interval-unions (cdr ls)(append rslt (list (find-interval-unions-help (car ls) (cdr ls)))))])))
;(define find-interval-unions-help (lambda (node ls) (cond [(null? ls) node] [else (find-interval-unions-help (if (interval-intersects? node (car ls)) (car(interval-union node (car ls))) node)   (cdr ls))])))







;(if (interval-intersects? node (car ls)) (interval-union node (car ls)) '())

;for each interval x (for each interval y in cdr (if intersects (add-to-result-set (union of x and y intervals)) else if not intersects, add-to-result-set (x)))







;#2
(define exists? (lambda (pred ls) (cond  [(null? ls) #f][(pred (car ls)) #t][else (exists? pred (cdr ls))])))






;#3
(define list-index (lambda (pred ls) (list-index-help pred ls 0)))
(define list-index-help (lambda (pred ls index)(cond  [(null? ls) #f][(pred (car ls)) index][else (list-index-help pred (cdr ls) (+ index 1))])))

;#4
(define pascal-triangle (lambda (n) (cond [(< n 0) '()][(equal? n 0) '((1))][(equal? n 1) '((1 1) (1))][else (pascal-triangle-help (- n 2) '((1 1) (1)))])))
(define pascal-triangle-help (lambda (n ls)(cond [(equal? 0 n) (append (list (build-pascal-list (car ls))) ls)][else (pascal-triangle-help (- n 1) (append (list (build-pascal-list (car ls))) ls) )])))
(define build-pascal-list (lambda (ls)  (append '(1) (build-pascal-list-help ls) '(1)) ))
(define build-pascal-list-help (lambda (ls)  (cond [(null? (cdr ls)) '()][else (append (list (+ (car ls) (cadr ls))) (build-pascal-list-help (cdr ls))  )])    ))



;#5
(define product (lambda (set1 set2) (cond [(null? set1) '()][(null? set2) '()][else (product-help set1 set2 '())])))
(define product-help (lambda (s1 s2 rslt) (apply append (map (lambda (val) (map (lambda (e) (list val e)) s2)) s1) )))


;(map (lambda (e) (list val e)) s2)

;iterate through all vals in s1 compining all vals in s2 with them.  Concat each time that you get a new list

;#6
(define max-edges (lambda (n) (cond [(= n 1) 0][(= n 0) 0] [else (/ (* n (- n 1)) 2)] )))

;#7
(define contains? (lambda (e s2) (if (null? s2) #f (if (equal? e (car s2)) #t (contains? e (cdr s2))))))
(define get-cars (lambda (l) (map (lambda (e) (car e))  l )))
(define complete? (lambda (g) (andmap (lambda (node) (andmap (lambda (e) (contains? e (cons (car node)(cadr node) ))) (get-cars g) )) g) ))

;#8
(define complete (lambda (l)  (map (lambda (e) (list e (complete-help l '() e))) l) ))
;(define complete (lambda (l)  (map (lambda (e) (list e '())) l) ))
(define complete-help (lambda (l rslt e)    (cond [(null? l) rslt][(equal? (car l) e) (complete-help (cdr l) rslt e)][else (complete-help (cdr l) (append rslt (list (car l))) e)])      ))


;#9
(define replace (lambda (old new ls) (cond [(null? ls) ls][(equal? old (car ls)) (append (list new)   (replace old new (cdr ls))  )][else (append (list (car ls))   (replace old new (cdr ls))  )])))



;#10
(define remove-first (lambda (element ls) (cond [(null? ls) ls][(equal? element (car ls)) (cdr ls)][else (append (list (car ls))   (remove-first element (cdr ls))  )])))


;#11
(define ls-reverse (lambda (ls) (cond [(null? ls) ls][else (append (ls-reverse (cdr ls))(list (car ls)))])))
(define remove-last (lambda (element ls) (ls-reverse (remove-first element (ls-reverse ls)))))
