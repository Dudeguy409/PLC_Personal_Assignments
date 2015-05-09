;Andrew Davidson
;Assignment 9

;#1

(define snlist-recur(lambda (base ls-proc e-proc) (letrec ([helper   (lambda (snls)   (cond[(not (list? snls) ) (e-proc snls)][(null? snls) base][else (ls-proc (map (lambda (e) (helper e) )   snls )   ) ])    )   ]) helper  )  ))





 (define sn-list-reverse (snlist-recur '() reverse (lambda (e) e) ))

 (define sn-list-paren-count (snlist-recur 2 (lambda (e) (+ 2 (apply + e))) (lambda (e) 0) ))
  (define sn-list-sum (snlist-recur 0 (lambda (e)  (apply + e)) (lambda (e) e) ))

  (define sn-list-map (lambda (proc ls) ((snlist-recur  '()  (lambda (e) e) proc ) ls) ))


  (define sn-list-occur (lambda (s ls) ((snlist-recur 0 (lambda (e)  (apply + e)) (lambda (e) (if (equal? e s) 1 0)) ) ls) ))





 (define sn-list-depth (snlist-recur 1 (lambda (e)  (+ 1(apply max e))) (lambda (e) 0) ))


;#2

(define bt-recur (lambda (node-proc e-proc) (letrec ([helper  (lambda (bt)   

	(cond[(number? bt) (e-proc bt)][else (node-proc (car bt) (map helper (cdr bt)))])



 )])   helper) ))

(define bt-sum (bt-recur (lambda (sym ls) (apply + ls))  (lambda (e) e) ))

(define bt-inorder (bt-recur (lambda (sym ls) (append (car ls) (list sym) (cadr ls)) )  (lambda (e) '()) ))