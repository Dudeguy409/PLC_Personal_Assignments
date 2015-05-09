;Andrew Davidson
;Madison Bruner

;Assignment 13

(load "chez-init.ss") 

		
;-------------------+
;                   |
;    PARSER         |
;                   |
;-------------------+

(define quote? (lambda (x) (if (list? x) (if (equal? (car x) 'quote) #t #f) #f ) ))

(define last (lambda (l) (if (null? l) '() (if (null? (cdr l)) (car l)  (last (cdr l)))) ))

(define all-but-last (lambda (l) (if (null? l) '() (if (null? (cdr l)) '() (cons (car l) (all-but-last (cdr l))))) ))

(define split-improper-list 
	(lambda (x) 
		(cond 
			[(pair? x)  (cons (car x) (split-improper-list (cdr x) ) )]
			[else  (list x  )]
		) 
	)  
)

(define parse-exp 
	(lambda 
		(datum)
		(cond 
			[(number? datum) (lit-exp datum)]
			[(vector? datum) (lit-exp datum)]
			[(boolean? datum) (lit-exp datum)]
			[(string? datum) (lit-exp datum)]
			[(symbol? datum) (var-exp datum)]
			[(list? datum)
				(cond
					[(quote? datum)  (lit-exp datum) ]
					[(eqv?  (car datum) 'if)  
						(cond 
							[(null? (cdr datum)) (eopl:error 'parse-exp "There are no expressions in the if statement" datum)]
							[(null? (cddr datum)) (eopl:error 'parse-exp "There is no then-expression in the if statement" datum)]
							[(null? (cdddr datum)) (if-exp (parse-exp (cadr datum)) (parse-exp (caddr datum))) ]
							[(null?  (cddddr datum) )  (if-else-exp (parse-exp (cadr datum)) (parse-exp (caddr datum))  (parse-exp (cadddr datum)) ) ]
							[else (eopl:error 'parse-exp "There are too many expressions in the if statement" datum)]
						)
						
					]
					[(eqv? (car datum) 'lambda)
						(let  ([body  (if (null? (cddr datum)) (eopl:error 'parse-exp "no body contained in the lambda expression" datum) (map parse-exp (cddr datum)) ) ]) 
							(cond 
								[(symbol? (cadr datum)) 
									(lambda-var-exp (parse-exp (cadr datum)) body)  
								]
								[(not (list? (cadr datum)))   
									(let
										([args (split-improper-list (cadr datum))]) 
											(let  
												(
													[required (all-but-last args)]
													[optional (last args)]
												)  
													(lambda-improper-exp (map check-params required) (check-params optional) body)
											)
									)
								]
								[else 
									(lambda-exp (map check-params (cadr datum) ) body)  
								]  
							)
						 )
					]
					[(eqv? (car datum) 'let*)
						(let-star-exp
							(if (not (list? (cadr datum))) (eopl:error 'parse-exp "list of tuples in the let* expression is not a list" datum) (map parse-tuple (cadr datum)))
							(if (null? (cddr datum)) (eopl:error 'parse-exp "no body contained in the let* expression" datum) (map parse-exp (cddr datum)) )
						)
					]
					[(eqv? (car datum) 'set!)
						(if (not (equal? 3  (length datum)) ) (eopl:error 'parse-exp "set! has the wrong number of parameters" datum) 
							(set-bang-exp
								(if (symbol? (cadr datum) ) (parse-exp (cadr datum))  (eopl:error 'parse-exp "the variable to be set in set! is not actually a variable" datum) )
								(parse-exp (cddr datum))
							)
						)
					]
					[(eqv? (car datum) 'letrec)
						(letrec-exp
							(if (not (list? (cadr datum))) (eopl:error 'parse-exp "list of tuples in the letrec expression is not a list" datum) (map parse-tuple (cadr datum)))
							(if (null? (cddr datum)) (eopl:error 'parse-exp "no body contained in the letrec expression" datum) (map parse-exp (cddr datum)) )
						)
					]
					[(eqv? (car datum) 'let)
						(cond 
							[(not (list? (cadr datum) ))  
							(let 
								([name (parse-exp (cadr datum))]) 
								(cases expression name 
									[var-exp (id)  
										(named-let-exp
											name
											(if (not (list? (cadr datum))) (eopl:error 'parse-exp "list of tuples in the named let expression is not a list" datum) (map parse-tuple (cadr datum)))
											(if (null? (cdddr datum)) (eopl:error 'parse-exp "no body contained in the let expression" datum) (map parse-exp (cdddr datum)) )
										)
							 		][else (eopl:error 'parse-exp "the name for the named-let was not a symbol" datum)]
							 	) 
							)
							]
							[else
								(let-exp
									(if (not (list? (cadr datum))) (eopl:error 'parse-exp "list of tuples in the let expression is not a list" datum) (map parse-tuple (cadr datum)))
									(if (null? (cddr datum)) (eopl:error 'parse-exp "no body contained in the let expression" datum) (map parse-exp (cddr datum)) )
								)
							]
						)
					]
					[else (app-exp (parse-exp (car datum))(map parse-exp (cdr datum)))]
				)
			]
			[else (eopl:error 'parse-exp "Invalid concrete syntax ~s" datum)]
		)
	)
)

(define parse-tuple 
	(lambda (node)
		(if 
			(or  (not(list? node)) (not (equal? (length node) 2)) (not (symbol? (car node)) )) 
			(eopl:error 'parse-exp "incorrect tuple structure" node)  
			(var-exp-tuple (car node) (parse-exp (cadr node)))
		)
	)
)

(define check-params 
	(lambda (p) 
		(if (not (symbol? p) )  
			(eopl:error 'parse-exp "parameters must be symbols" p)    
			(parse-exp p) 
		)
	)
)



(define 
	unparse-exp
	(lambda
		(exp)
		(cases expression exp
			[var-exp (id) id]
			[lit-exp (id) id]
			[set-bang-exp (id exp)  (list 'set! (unparse-exp id) (unparse-exp exp)  ) ]
			[if-exp (exp1 exp2)  (list 'if (unparse-exp exp1) (unparse-exp exp2) ) ]
			[if-else-exp (exp1 exp2 exp3)  (list 'if (unparse-exp exp1) (unparse-exp exp2)  (unparse-exp exp3)  )]
			[lambda-exp (id body) (append (list 'lambda (map unparse-exp id) ) (map unparse-exp body))]
			[lambda-var-exp (id body) (append (list 'lambda (unparse-exp id) ) (map unparse-exp body))]
			[lambda-improper-exp  (required optional body) (list require optional body)]
			[named-let-exp (name id body) (append (list 'let (unparse-exp name) (map unparse-tuple id) ) (map unparse-exp body))]
			[let-exp (id body) (append (list 'let (map unparse-tuple id) ) (map unparse-exp body))]
			[let-star-exp (id body) (append (list 'let* (map unparse-tuple id) ) (map unparse-exp body))]
			[letrec-exp (id body) (append (list 'letrec (map unparse-tuple id) ) (map unparse-exp body))]
			[app-exp (rator rand)(append (list (unparse-exp rator)  ) (map unparse-exp rand)) ]
		)
	)
)

(define unparse-tuple (lambda (tuple-exp) (cases tuple tuple-exp  [var-exp-tuple (id exp) (list id (unparse-exp exp))]) ))

;------------------------------------------------------------------------------------------------------------------------------------------



;-------------------+
;                   |
;    DATATYPES      |
;                   |
;-------------------+

;; environment type definitions

(define scheme-value?
  (lambda (x) #t))

(define-datatype environment environment?
	[empty-env-record]
	[extended-env-record
		(syms (list-of symbol?))
		(vals (list-of scheme-value?))
		(env environment?)
	]
)


; datatype for procedures.  At first there is only one
; kind of procedure, but more kinds will be added later.

(define-datatype proc-val proc-val?
  [prim-proc
   (name symbol?)]
   [closure (args (list-of expression?)) (exp (list-of expression?)) (env environment?)]
   [lambda-var-closure (arg expression?) (exp (list-of expression?)) (env environment?)]
   [lambda-improper-closure (required (list-of expression?)) (optional expression?) (exp (list-of expression?)) (env environment?)]
 )



(define-datatype 
	expression  
	expression? 
		[lit-exp (id (lambda (x) (or (vector? x) (number? x) (boolean? x) (string? x)  (quote? x) ) ))]
		[var-exp (id symbol?)]
		[set-bang-exp  (id symbol?)  (exp expression?) ]
		[lambda-exp (id (list-of expression?)) (body (list-of expression?))]
		[lambda-var-exp (id expression?) (body (list-of expression?))]
		[lambda-improper-exp (required  (list-of expression?)) (optional expression?) (body (list-of expression?))]
		[app-exp (rator expression?) (rand (list-of expression?))]
		[named-let-exp (name expression?) (id (list-of tuple?)) (body (list-of expression?)) ]
		[let-exp  (id (list-of tuple?)) (body (list-of expression?)) ]
		[let-star-exp  (id (list-of tuple?)) (body (list-of expression?)) ]
		[letrec-exp  (id (list-of tuple?)) (body (list-of expression?)) ]
		[if-exp (exp1 expression?) (exp2 expression?) ]
		[if-else-exp (exp1 expression?) (exp2 expression?)  (exp3 expression?) ]
)

(define-datatype 
	tuple  
	tuple? 
		[var-exp-tuple (id symbol?)(exp expression?)]
)

(define get-tuple-id (lambda (tup) (cases tuple tup [var-exp-tuple (id exp) id])))
(define get-tuple-exp (lambda (tup) (cases tuple tup [var-exp-tuple (id exp) exp])))
	 






;-------------------+
;                   |
;   ENVIRONMENTS    |
;                   |
;-------------------+





; Environment definitions for CSSE 304 Scheme interpreter.  Based on EoPL section 2.3

(define empty-env
  (lambda ()
    (empty-env-record)))

(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms vals env)))

(define list-find-position
  (lambda (sym los)
    (list-index (lambda (xsym) (eqv? sym xsym)) los)))

(define list-index
  (lambda (pred ls)
    (cond
     ((null? ls) #f)
     ((pred (car ls)) 0)
     (else (let ((list-index-r (list-index pred (cdr ls))))
	     (if (number? list-index-r)
		 (+ 1 list-index-r)
		 #f))))))

(define apply-env
  (lambda (env sym succeed fail) ; succeed and fail are procedures applied if the var is or isn't found, respectively.
    (cases environment env
      (empty-env-record ()
        (fail))
      (extended-env-record (syms vals env)
	(let ([pos (list-find-position sym syms)])
      	  (if (number? pos)
	      (succeed (list-ref vals pos))
	      (apply-env env sym succeed fail)))))))









;-----------------------+
;                       |
;   SYNTAX EXPANSION    |
;                       |
;-----------------------+



(define syntax-expand
	(lambda (exp) 
		(cases expression exp
			[let-exp (id body)
				(let
					(
						[syms  (map get-tuple-id-exp id)]
						[vals (map get-tuple-exp id)]
					)
						(app-exp (lambda-exp syms (map syntax-expand body)) (map syntax-expand vals) )
				)
			]
			[lit-exp (id)   exp]
			[var-exp (id)  exp]
			[set-bang-exp  (id body)  (set-bang-exp id (syntax-expand body))]
			[lambda-exp (id body) (lambda-exp (map syntax-expand id)  (map syntax-expand body) )]
			[lambda-var-exp (id body) (lambda-var-exp  (syntax-expand id)  (map syntax-expand body) )]
			[lambda-improper-exp  (required optional body) (lambda-improper-exp  (map syntax-expand required) (syntax-expand optional)  (map syntax-expand body)  )]
			[app-exp  (rator rand)
				(cond 
					[(equal? (cadr rator) 'or )  
						(cond 
							[(null? rand)  
								(lit-exp #f)
							]
							[else 
								(if-else-exp 
									(car rand) 
									(car rand)
									(syntax-expand  
										(app-exp 
											(var-exp 'or) 
											(cdr rand)  
										) 
									) 
								)
							]
						)
					]
					[(equal? (cadr rator) 'cond)  
						(cond 
							[(null? rand)  
								(lambda () (eopl:error 'syntax-expand "cond should not be empty: ~s"rand))
							]
							[(null? (cdr rand)) (syntax-expand (car (caddr (car rand) ) ) )]
							[else 
								(if-else-exp 
									 (syntax-expand (cadr(car rand)) )
									 (syntax-expand  (car (caddr (car rand))))
									(syntax-expand  
										(app-exp 
											(var-exp 'cond) 
											(cdr rand)  
										) 
									) 
								)
							]
						)
					]
					[(equal? (cadr rator) 'begin)  
						(lambda-exp '() rand)
					]
					[(equal? (cadr rator) 'case)  
						4
					]
					[else 
						(app-exp (syntax-expand rator)  (map syntax-expand rand))
					]
				) 
			]
			;TODO parse tuple
			[named-let-exp (name id body) (named-let-exp (syntax-expand name) id (map syntax-expand body))]
			[let-star-exp (id body)   
				;(let-star-exp id (map syntax-expand body) )
				(syntax-expand  
					(let-exp   
						(car id)  
						(syntax-expand  
							(let-star-exp  
								(cdr id) 
								body
							)
						)  
					) 
				)
			]
			[letrec-exp  (id body)  (letrec-exp id (map syntax-expand body) )]
			[if-exp (exp1 exp2) (if-exp (syntax-expand exp1) (syntax-expand exp2)  )]
			[if-else-exp (exp1 exp2 exp3) (if-else-exp (syntax-expand exp1) (syntax-expand exp2) (syntax-expand exp3))]
		)  
	) 
)

(define get-tuple-id-exp (lambda (x)  (var-exp (get-tuple-id  x) )  ) )









;-------------------+
;                   |
;   INTERPRETER     |
;                   |
;-------------------+



; top-level-eval evaluates a form in the global environment

(define top-level-eval
  (lambda (form)
    ; later we may add things that are not expressions.
    (eval-exp form init-env)))

; eval-exp is the main component of the interpreter
; look up its value.
; procedure to call if id not in env
; procedure to call if id is in the environment
(define eval-exp
	(lambda 
		(exp env)
		(cases expression exp
			[lit-exp (datum)
				(if (quote? datum) (cadr datum)  datum)
			]
			[set-bang-exp  (id exp) (set! id (eval-exp exp env)) ]
			[lambda-exp (id body) (closure id body env)]
			[lambda-var-exp (id body) (lambda-var-closure id body env)]
			[lambda-improper-exp  (required optional body) (lambda-improper-closure required optional body env)]
			[let-exp (id body) 
				(let 
					(
						[syms 
							(map get-tuple-id id)
						]
						[vals 
							(map 
								(lambda (x) (eval-exp x env))
								(map get-tuple-exp id)
							)
						]
					) 
						(let 
							(
								[new-env (extend-env syms vals env)]
							)
								(car 
									(reverse(map 
										(lambda (x) (eval-exp x new-env))
										body
									))
								)
						)	
				)
			]
			[var-exp (id)
				(apply-env 
					env 
					id 
					(lambda (x) x)  
					(lambda () (eopl:error 'apply-env "variable not found in environment: ~s"id))
				)
			]
			[app-exp (rator rands)
				(let
					(
						[proc-value (eval-exp rator env)]
						[args (eval-rands rands env)]
					)
						(apply-proc proc-value args) )]
			[if-exp (exp1 exp2) (if (eval-exp exp1 env) (eval-exp exp2 env))]
			[if-else-exp (exp1 exp2 exp3) (if (eval-exp exp1 env) (eval-exp exp2 env) (eval-exp exp3 env) )]
			[else (eopl:error 'eval-exp "Bad abstract syntax: ~a" exp)]
		)
	)
)

; evaluate the list of operands, putting results into a list

(define eval-rands
  (lambda 
  	(rands env)
    	(map 
    		(lambda 
    			(x) 
    			(eval-exp x env)
    		)
    		rands
    	)
    )
 )


;  Apply a procedure to its arguments.
;  At this point, we only have primitive procedures.  
;  User-defined procedures will be added later.
;TODO may need to re-parse arguments sent into app-exp
(define apply-proc
  (lambda (proc-value args)
    (cases proc-val proc-value
      [prim-proc (op) (apply-prim-proc op args)]
      [closure (syms exp env)  (apply-closure-proc syms args exp env)]
      [lambda-var-closure (sym exp env)  (apply-closure-proc (list sym) (list args) exp env )]
      [lambda-improper-closure (required optional exp env)  
      	(let 
      		(
      			[split-ls (split-passed-in-args args (length required))]
      		) 
      			(apply-closure-proc required 
      				(car split-ls) 
      				(list(app-exp 
      					(lambda-var-exp optional exp)   
      					(map parse-exp (cadr split-ls)  )      
      				))   
      				env
      			) 
      	) 
      ]
			; You will add other cases
      [else (eopl:error 'apply-proc
                   "Attempt to apply bad procedure: ~s" 
                    proc-value)])))

(define *prim-proc-names* '(+ - * add1 sub1 cons = equal? vector eq? car cadar > < cdr cadr caar list list? length number? >=  <= symbol? vector? vector->list list->vector pair? not procedure? zero? / null? set-car! set-cdr! vector-ref apply map))

(define init-env         ; for now, our initial global environment only contains 
  (extend-env            ; procedure names.  Recall that an environment associates
     *prim-proc-names*   ;  a value (not an expression) with an identifier.
     (map prim-proc      
          *prim-proc-names*)
     (empty-env)))


(define apply-closure-proc 
	(lambda 
		(syms vals exps env) 
			(let 
				(
					[new-env (extend-env (map unparse-exp syms) vals env)]
				) 	
					(car
						(reverse
							(map 
								(lambda (x) (eval-exp x new-env))
								exps
							)
						)
					) 
			)
	)
)
; Usually an interpreter must define each 
; built-in procedure individually.  We are "cheating" a little bit.

(define apply-prim-proc
	(lambda (prim-proc args)
		(case prim-proc
			[(+) (apply + args)]
			[(-) (apply - args)]
			[(*) (apply * args)]
			[(add1) (+ (car args) 1)]
			[(sub1) (- (car args) 1)]
			[(cons) (cons (car args) (cadr args))]
			[(=) (= (car args) (cadr args))]
			[(not) (not (car args) )]
			[(equal?) (equal? (car args) (cadr args) )]
			[(>=)  (>= (car args) (cadr args) )]
			[(<=)  (<= (car args) (cadr args) )]
			[(>)  (> (car args) (cadr args) )]
			[(<)  (< (car args) (cadr args) )]
			[(car)  (apply car args)]
			[(cdr) (apply cdr args)]
			[(list)  args]
			[(list?) (apply list? args) ]
			[(eq?) (eq? (car args) (cadr args) ) ]
			[(length) (apply length args)]
			[(pair?)  (pair?  args)]
			[(vector->list) (apply vector->list args)]
			[(list->vector) (apply list->vector args)]
			[(vector?) (apply vector? args)]
			[(number?) (apply number?  args) ]
			[(symbol?) (apply symbol? args) ]
			[(caar) (apply caar args)]
			[(cadr) (apply cadr args)]
			[(cadar) (apply cadar args)]
			[(null?) (apply null? args)]
			[(procedure?)(proc-val? (car args))]
			[(zero?) (zero? (car args))]
			[(/) (apply / args)]
			[(vector-ref) (apply vector-ref args)]
			[(vector) (apply vector args)]
			[(set-cdr!) (set-cdr! (car args) (cadr args))]
			[(set-car!) (set-car! (car args) (cadr args))]
			[(map) (map (lambda (x) (apply-proc (car args) (list x))  )  (cadr args) ) ]
			[(apply) (apply-proc (car args) (cadr args) )]
			[else (error 'apply-prim-proc "Bad primitive procedure name: ~s" prim-proc)]
		)
	)
)

(define rep      ; "read-eval-print" loop.
  (lambda ()
    (display "--> ")
    ;; notice that we don't save changes to the environment...
    (let ([answer (top-level-eval (parse-exp (read)))])
      ;; TODO: are there answers that should display differently?
      (eopl:pretty-print answer) (newline)
      (rep))))  ; tail-recursive, so stack doesn't grow.

(define eval-one-exp
	(lambda (x) 
		(top-level-eval 
			(syntax-expand 
				(parse-exp x) 
			) 
		)
	)
)

(define split-passed-in-args 
	(lambda  (ls len)   
		(cond 
			[(equal? len 0) (list '() ls)]
			[else 
				(let 
					(
						[rslt (split-passed-in-args (cdr ls)  (- len 1))]
					)  
					(list (cons (car ls) (car rslt)) (cadr rslt) )  
				) 
			] 
		)  
	)  
)