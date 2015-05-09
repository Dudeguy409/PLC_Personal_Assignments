; Andrew Davidson
; Assignment 3

; #1
;this procedure passes in the original point and points list plus a default ""shortest-length-so-far" to be used and an empty list representing the point found
(define nearest-point (lambda (p list-of-points) (nearest-point-help p list-of-points (+ (distance p (car list-of-points)) 1) '())))
(define nearest-point-help (lambda (p list-of-points shortest-length-found point-found) (if (equal? '() list-of-points) point-found (if (< (distance p (car list-of-points)) shortest-length-found) (nearest-point-help p (cdr list-of-points) (distance p (car list-of-points)) (car list-of-points)) (nearest-point-help p (cdr list-of-points) shortest-length-found point-found)))))
(define distance (lambda (p1 p2)  (vec-length (make-vec-from-points p1 p2))   ))
(define make-vec-from-points (lambda (p1 p2)  (list (- (1st p2) (1st p1))  (- (2nd p2) (2nd p1))  (- (3rd p2)(3rd p1)))   ))
(define vec-length (lambda (v)  (sqrt (+ (* (1st v) (1st v))  (* (2nd v) (2nd v))  (* (3rd v)(3rd v))))   ))
(define 1st (lambda (m) (car m)))
(define 2nd (lambda (m) (cadr m)))
(define 3rd (lambda (m) (caddr m)))

;#2
(define  union (lambda (a b) (if (set-empty? b) a (union (union-add a (car b)) (cdr b)))))
(define union-add (lambda (a e) (if (union-add-help? a e) (cons e a) a)))
(define union-add-help? (lambda (a e) (if (set-empty? a) #t (if (equal? e (car a)) #f (union-add-help? (cdr a) e)))))
(define set-empty? (lambda (s) (equal? s '())))

;#3
(define intersection (lambda (s1 s2) (intersection-help s1 s2 '())))
(define intersection-help (lambda (s1 s2 rslt) (if (set-empty? s1) rslt (intersection-help (cdr s1) s2 (intersection-add (car s1) s2 rslt)))))
(define intersection-add (lambda (e s2 rslt)(if (set-empty? s2) rslt (if (equal? e (car s2)) (cons e rslt) (intersection-add e (cdr s2) rslt)))))

;#4
(define subset? (lambda (s1 s2) (if (set-empty? s1) #t (and (contains? (car s1) s2) (subset?(cdr s1) s2) ))))
(define contains? (lambda (e s2) (if (set-empty? s2) #f (if (equal? e (car s2)) #t (contains? e (cdr s2))))))

;#5
(define relation? (lambda (l) (if (set? l) (two-elements? l) #f)))
(define two-elements? (lambda (l) (if (set-empty? l) #t (and (two-elements-help? (car l)) (two-elements? (cdr l))))))
(define two-elements-help? (lambda (l) (if (list? l) (if (set-empty? l) #f (if (set-empty? (cdr l)) #f (if (set-empty? (cddr l))#t #f)))#f)))
(define set? (lambda (l) (if (list? l)  (if (equal? '() l) #t (and (set-help? (cdr l) (car l)) (set? (cdr l)))) #f)))
(define set-help? (lambda (l e) (if (equal? '() l) #t(if(equal? e (car l)) #f (set-help? (cdr l) e)))))

;#6
(define domain (lambda (l) (if (relation? l) (domain-help l '()) '())))
(define domain-help (lambda (l rslt) (if (set-empty? l) rslt (domain-help (cdr l) (union rslt (list (1st (car l))))))))

;#7
(define reflexive? (lambda (r) (reflexive-help? r r)))
(define reflexive-help? (lambda (r s) (if (set-empty? s) #t (if (reflexive-check? r (car s)) (reflexive-help? r (cdr s)) #f))))
(define reflexive-check? (lambda (r e) (and (contains? (list (car e) (car e)) r) (contains? (list (cadr e) (cadr e)) r))))

;#8
(define even? (lambda (n) (equal? 0 (modulo n 2))))
(define even-algorithm (lambda (n) (/ n 2)))
(define odd-algorithm (lambda (n) (+ (* 3 n) 1)))
(define hailstone-step-count (lambda (n) (hailstone-step-count-help n 0)))
(define hailstone-step-count-help (lambda (n c) (if (equal? n 1) c (if (even? n) (hailstone-step-count-help (even-algorithm n) (+ c 1))  (hailstone-step-count-help (odd-algorithm n) (+ c 1))))))

