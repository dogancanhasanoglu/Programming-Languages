(define get-operator 
	(lambda (op-symbol env) 
		(cond 
			((equal? op-symbol '+) +)
			((equal? op-symbol '-) -)
			((equal? op-symbol '*) *)
			((equal? op-symbol '/) /)
			(else  (display "cs305: ERROR\n\n") (repl env))
		)

	)
)

;; get-operator taken from s6.scm

(define define-stmt? 
  (lambda (e)
     (and 
       (list? e) 
       (= (length e) 3) 
       (eq? (car e) 'define)
       (symbol? (cadr e))
     )
  )
)

;; define-stmt? taken from s6.scm

(define cond-check? 
	(lambda (e)
		(if (null? e) 
			#f
			(if (and (list? (car e)) (= (length (car e)) 2)) ;; checks if there is even a valid expression or not
				(if (equal? (caar e) 'else) ;; if it is equal to else remaining must be null
					(if (null? (cdr e)) #t 
						#f
					)
					(cond-check? (cdr e)) ;; proceeds to remaining of the list
				)
				#f
			)
		)
	)	
)

(define if-stmt? 
	(lambda (e)
		(and (list? e) (equal? (car e) 'if) (= (length e) 4))
	)
)

;; checks if it is if statement or not

(define cond-stmt? 
	(lambda (e)
		(and (list? e) (equal? (car e) 'cond) (> (length e) 2) (cond-check? (cdr e)))
	
	)
)

;; checks if it is conditional statement or not
;; it sends the list without "cond" keyword to check for validity in cond-check? procedure

(define let-check? 
	(lambda (e)
		(if (not(list? e))  ;; only accepts list form
			#f
			(if (null? e)   ;; null is okay
				#t
				(if (= (length (car e)) 2)  ;; if there are more or less than 2 elements then expression cannot be true
					(let-check? (cdr e)) ;; proceeds to remaining of the list
					#f
				)
			)
		)
	)
)

(define letstar-stmt? 
	(lambda (e)
		(and (list? e) (equal? (car e) 'let*) (= (length e) 3) (let-check? (cadr e)))
	
	)
)

;; letstar-stmt? for checking whether the input is in type of a let statement
;; it sends the list without "let" keyword to check for validity in let-check? procedure

(define let-stmt? 
	(lambda (e)
		(and (list? e) (equal? (car e) 'let) (= (length e) 3) (let-check? (cadr e)))
	
	)
)

;; let-stmt? for checking whether the input is in type of a let* statement
;; it sends the list without "let*" keyword to check for validity in let-check? procedure

(define update-env 
	(lambda (var val old-env)
		(cons (cons var val) old-env)
	)
)

;; update-env for adding the new variable to the beginning of the old environment

(define get-value 
	(lambda (var old-env new-env)
		(cond
			((null? new-env) (display "cs305: ERROR\n\n") (repl old-env))

			((equal? (caar new-env) var) (cdar new-env))

			(else (get-value var old-env (cdr new-env)))
		)
	)
)

;; get-value taken from s6.scm with a little modification

(define repl 
	(lambda (env)
		(let* (
         
			(dummy1 (display "cs305> "))
			(expr (read))
			(new-env (if (define-stmt? expr)
						(update-env (cadr expr) (s7-interpret (caddr expr) env) env)
                      env
					  )
			)
			(val 	(if (define-stmt? expr)
					  (cadr expr)
					  (s7-interpret expr env)
					)
			)
			(dummy2 (display "cs305: "))
		    (dummy3 (display val))
		    (dummy4 (newline))
			(dummy4 (newline)))
			(repl new-env)
		)
	)
)

;; repl taken from s6.scm

(define s7-interpret 
	(lambda (e env)
		(cond 
    
			((number? e) e)  ;; if it is number then return right away

    
			((symbol? e) (get-value e env env))  ;; if it is symbol then return the value binding of that variable

    
			((not (list? e)) 
				(display "cs305: ERROR\n\n") (repl env) )  ;; if it is not a list it is problematic, reject it

   
			((null? e) e)   ;; null is okay

    
			((if-stmt? e) (if (eq? (s7-interpret (cadr e) env) 0)  ;; if statement
					( s7-interpret (cadddr e) env)
					( s7-interpret (caddr e) env)))
  
   
			((cond-stmt? e)     ;; conditional statement
				(if (eq? (length e) 3) 
					(if (eq? (s7-interpret (caadr e) env) 0) (s7-interpret (car (cdaddr e)) env)  ;; for basic type of conditional statement such as 1 if 1 else
						(s7-interpret (cadadr e) env)
					)
					
					(let ((if-cond  (caadr e)) (then (cadadr e)) (else-part (cons 'cond (cddr e))))   ;; otherwise parse condition into nested if statements
						(let ((c (list 'if if-cond then else-part))) (s7-interpret c env))
					)
				)
			)

			
			((let-stmt? e)  ;; let statement
			    (let ((names (map car  (cadr e)))
					(exprs (map cadr (cadr e)))
					)
				    (let ((vals (map (lambda (expr) (s7-interpret expr env)) exprs)))
					(let ((new-env (append (map cons names vals) env)))
						(s7-interpret (caddr e) new-env))
					)
				)
			)


			((letstar-stmt? e)      ;; let* statement
				(if (<= (length (cadr e)) 1)    ;; for not having more than 1 element
					(let ((l (list 'let (cadr e) (caddr e))))   
						(let ((names (map car  (cadr e)))
								(exprs (map cadr (cadr e))))
							(let ((vals (map (lambda (expr) (s7-interpret expr env)) exprs)))
								(let ((new-env (append (map cons names vals) env)))
									(s7-interpret (caddr e) new-env)
								)
							)
						)
					)
					
					;; or else parse into nested let statements
					(let ((first (list 'let (list (caadr e)))) (rest (list 'let* (cdadr e) (caddr e))))
						(let ((l (append first (list rest)))) 
							(let ((names (map car (cadr l))) (inits (map cadr (cadr l))))
								(let ((vals (map (lambda (init) (s7-interpret init env)) inits)))
									(let ((new-env (append (map cons names vals) env)))
										(s7-interpret (caddr l) new-env))
									)
							)
						)
					)
				)
			)
			 
			(else    ;;same as s6.scm
				(let 
					((operands (map s7-interpret (cdr e) (make-list (length (cdr e)) env)))
					(operator (get-operator (car e) env)))
					(apply operator operands)
				)
			)
	  
	  
		)
	)
)

(define cs305 (lambda () (repl '())))
