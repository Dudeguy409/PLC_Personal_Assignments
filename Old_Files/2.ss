;  Andrew Davidson, Assignment 2


;#1a - factorial
(define fact (lambda (m) (fact-help m 1)))
(define fact-help (lambda (m n) (if (= m 0) n (fact-help (- m 1) (* m n)))))

; #1b - choose
(define choose (lambda (n k) (/ (fact n) (* (fact k)(fact (- n k))))))

; #2 - range
(define range (lambda (m n) (if (>= m n) '()   (cons m (range (+ m 1) n )))))

; #3 - set
(define set? (lambda (l) (if (equal? '() l) #t (and (set-help? (cdr l) (car l)) (set? (cdr l))))))
(define set-help? (lambda (l e) (if (equal? '() l) #t(if(equal? e (car l)) #f (set-help? (cdr l) e)))))

; #4 - sum of squares
(define sum-of-squares (lambda (lon) (if (equal? '() lon) 0 (+ (* (car lon) (car lon)) (sum-of-squares (cdr lon))) )))

; #utility methods for #5-11
(define 1st (lambda (m) (car m)))
(define 2nd (lambda (m) (cadr m)))
(define 3rd (lambda (m) (caddr m)))

; #5 - make vector from two points
(define make-vec-from-points (lambda (p1 p2)  (list (- (1st p2) (1st p1))  (- (2nd p2) (2nd p1))  (- (3rd p2)(3rd p1)))   ))

; #6 - dot product
(define dot-product (lambda (v1 v2)  (+ (* (1st v2) (1st v1))  (* (2nd v2) (2nd v1))  (* (3rd v2)(3rd v1)))   ))

; #7 - vector length
(define vec-length (lambda (v)  (sqrt (+ (* (1st v) (1st v))  (* (2nd v) (2nd v))  (* (3rd v)(3rd v))))   ))

; #8 - distance between two points
(define distance (lambda (p1 p2)  (vec-length (make-vec-from-points p1 p2))   ))

; #9 - cross product
(define cross-product (lambda (v1 v2)  (list (- (* (2nd v1) (3rd v2)) (* (3rd v1) (2nd v2)))  (- (* (3rd v1) (1st v2)) (* (1st v1) (3rd v2)))  (- (* (1st v1) (2nd v2))(* (2nd v1) (1st v2))))   ))

; #10 - parallel
(define parallel? (lambda (v1 v2)  (= (/ (1st v2) (1st v1))  (/ (2nd v2) (2nd v1))  (/ (3rd v2)(3rd v1)))   ))

; #11 - collinear
(define collinear? (lambda (p1 p2 p3)  (parallel? (make-vec-from-points p1 p2) (make-vec-from-points p2 p3))   ))


