; Andrew Davidson
; Assignment 4

;#1
(define relation? (lambda (l) (if (set? l) (two-elements? l) #f)))
(define two-elements? (lambda (l) (if (set-empty? l) #t (and (two-elements-help? (car l)) (two-elements? (cdr l))))))
(define two-elements-help? (lambda (l) (if (list? l) (if (set-empty? l) #f (if (set-empty? (cdr l)) #f (if (set-empty? (cddr l))#t #f)))#f)))
(define set? (lambda (l) (if (list? l)  (if (equal? '() l) #t (and (set-help? (cdr l) (car l)) (set? (cdr l)))) #f)))
(define set-help? (lambda (l e) (if (equal? '() l) #t(if(equal? e (car l)) #f (set-help? (cdr l) e)))))
(define 1st (lambda (m) (car m)))
(define  union (lambda (a b) (if (set-empty? b) a (union (union-add a (car b)) (cdr b)))))
(define union-add (lambda (a e) (if (union-add-help? a e) (cons e a) a)))
(define union-add-help? (lambda (a e) (if (set-empty? a) #t (if (equal? e (car a)) #f (union-add-help? (cdr a) e)))))
(define multi-set? (lambda (ms) (if (set-empty? ms) #t (if (relation? ms) (if (domain-set? ms) (multi-set-help? ms) #f) #f) )))
(define multi-set-help? (lambda (ms) (if (set-empty? ms) #t (and (and (number? (cadr (car ms))) (> (cadr (car ms)) 0)) (multi-set-help? (cdr ms))))))
(define domain-set? (lambda (l)  (set? (domain-set-help? l '()))))
(define domain-set-help? (lambda (l rslt) (if (set-empty? l) rslt (domain-set-help? (cdr l) (cons  (1st (car l)) rslt)))))

;#2
(define ms-size (lambda (l)    (apply + (map   (lambda (e) (cadr e))   l  ))    ))

;#3
(define matrix-ref (lambda (l row col) (cond [(equal? row 0) (matrix-ref-help (car l) col)] [else(matrix-ref (cdr l) (- row 1) col)])))
(define matrix-ref-help (lambda (l col) (cond [(equal? col 0) (car l)][else (matrix-ref-help (cdr l) (- col 1))])))


;#4
(define l-size (lambda (l) (cond [(list? l) (cond [(set-empty? l) 0][(set-empty? (cdr l)) 1][else (1+(l-size (cdr l)))])][else #f])))
(define matrix? (lambda (l)  (cond [(not (list? l)) #f][(set-empty? l) #t][(set-empty? (cdr l)) #t][else (and(equal? (l-size (car l)) (l-size (cadr l))) (matrix? (cdr l)))])))



;#5
(define get-cdrs (lambda (l) (map (lambda (e) (cdr e))  l )))
(define get-cars (lambda (l) (map (lambda (e) (car e))  l )))
(define matrix-transpose (lambda (l)     (matrix-transpose-help l '())   ))
(define matrix-transpose-help (lambda (l rslt)     (if (set-empty? (car l)) rslt (matrix-transpose-help (get-cdrs l) (append rslt (list (get-cars l))))     )   ))



;#6
(define last (lambda (l) (if (set-empty? l) '() (if (set-empty? (cdr l)) (car l)  (last (cdr l)))) ))

;#7
(define set-empty? (lambda (s) (equal? s '())))
(define all-but-last (lambda (l) (if (set-empty? l) '() (if (set-empty? (cdr l)) '() (cons (car l) (all-but-last (cdr l))))) ))
