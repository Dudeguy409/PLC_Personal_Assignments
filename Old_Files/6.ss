;Andrew Davidson
;Assignment 6

;#1
(define curry2 (lambda (funct) (lambda (y) (lambda (x) (funct y x)))))

;#2
(define curried-compose (lambda (funct) (lambda (funct2) (lambda (ls) (funct (funct2 ls))))))




;#3


;#4


;#5



;#6



;#7
(define filter-in (lambda (pred? ls)  (apply append (map (lambda (e) (if (pred? e) (list e) '())) ls))))


;#8
(define filter-out (lambda (pred? ls)  (apply append (map (lambda (e) (if (pred? e) '() (list e) )) ls))))



;#9


;#10
(define invert (lambda (ls) (map reverse ls)))


;#11
(define vector-index (lambda (pred? vec) (vec-index-help pred? vec 0)))
(define vec-index-help (lambda (pred? vec index)(cond  [(equal? index (vector-length vec)) #f][(pred? (vector-ref vec index) ) index][else (vec-index-help pred? vec (+ index 1))])))

