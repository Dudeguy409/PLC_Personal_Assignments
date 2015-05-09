(define interval-contains? (lambda (m n)  (if (< n (car m)) #f (if (> n (cadr m)) #f #t))))

(define interval-intersects? (lambda (m n)  (if (interval-contains? m (car n))#t (if (interval-contains? m (cadr n))#t (if (interval-contains? n (car m))#t (if (interval-contains? n (cadr m))#t #f))))))


(define Fahrenheit->Celsius (lambda (m) (* 5/9 (- m 32))))

(define divisible-by-7? (lambda (m) (= 0 (modulo m 7))))


(define ends-with-7? (lambda (m) (= 7 (modulo m 10))))


(define 1st (lambda (m) (car m)))

(define 2nd (lambda (m) (cadr m)))

(define 3rd (lambda (m) (caddr m)))

(define find-min (lambda (m n) (if(< (car m) (car n)) (car m)  (car n))))

(define find-max (lambda (m n) (if(> (cadr m) (cadr n)) (cadr m)  (cadr n))))

(define interval-union (lambda (m n) 

(if (interval-intersects? m n) (list (list (find-min m n) (find-max m n))) (list m n))


))