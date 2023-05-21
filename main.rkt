;needed updates:
;dont print procedure for last 2 inputs of function 2.
;work on evaluating function - func part.
;revise and test 6.
;:= - last 1 input example.
;if - last 1 input example (false)
; if while func
; map eval outputu farklı
;continue working from function 7. 
;i guess there is a problem with calls like := a 5. it is not working.
;got stuck in eval-exprs.
;got stuck in while.
;got stuck in func.

; Basak Tepe
; 2020400117
; compiling: no
; complete: no
#lang racket


(provide (all-defined-out))

; read and parse the input file
(define parse (lambda (input-file)
        (letrec (
            [input-port (open-input-file input-file)]
            [read-and-combine (lambda ()
                (let ([line (read input-port)])
                    (if (eof-object? line)
                        '()
                        (append `(,line) (read-and-combine))
                    )
                )
            )]
            )
            (read-and-combine)
        )
    )
)
(define create-hash (lambda (vars values)
        (letrec (
            [create-hash-iter (lambda (vars values hash)
                (if (null? vars)
                    hash
                    (create-hash-iter (cdr vars) (cdr values) (hash-set hash (car vars) (car values)))
                )
            )]
            )
            (create-hash-iter vars values (hash))
        )
    )
)

(define add-to-hash (lambda (old-hash new-hash)
        (foldl (lambda (key hash) (hash-set hash key (hash-ref new-hash key)))
            old-hash
            (hash-keys new-hash)
        )
    )
)

(define eval-program (lambda (program-str)
        (get (eval-exprs (parse program-str) empty-state) '-r)
    )
)

; solution starts here
; 1. empty-state (5 points)
;(define empty-state 0)
(define empty-state
        (hash))



; 2. get (5 points)
;(define get 0)

;state is a hash. 
(define (get state var)
    (let ((val (hash-ref state var #f))) ;#f is the default value if the key is not found
    (if (not (eq? val #f)) ;if the key is found, val is not false.
        val
    (eval var)))) ;else (if key not found) evaluate the key.



; 3. put (5 points)
;(define put 0)
(define (put state var val)
    (let ((newstate (hash-set state var val))) ;update the value or create a new key-value pair
    newstate));return the new state




(define (map-eval lst state)
    (printf "THE LIST IS  ~a\n" lst);
    ;first element is the operator
    (let* ((operator (car lst))
            (operands (cdr lst))
            (results (map (lambda (operand) (get (eval-expr operand state) '-r)) operands))) ;map each element of list (exprs) to values. resut is a list of mappings.
            ;results is a pure numeric exp list. for example, ( 2 b) gets to the form of ( 2 6)
    (let ((new-expr (cons operator results))) ;add the operator back to the list.
    (printf "THE NEW EXPR IS  ~a\n" new-expr);
    (let ((newstate (put state '-r (eval new-expr)))) ;evaluate the new expression and put it to the state.
    (printf "THE NEW STATE IS  ~a\n" newstate);
 newstate)))) 

    ;(if (eq? #f (hash-ref newstate '-r)) ;if the value is false
     ;   ((printf "THE VALUE IS FALSE\n")
      ;  (let ((return-state (put newstate '-r 99999)));set it to some dummy -r 99999
       ; return-state)) ;return the new state. with -r variable updated.
        ;newstate))));return the new state. with -r variable updated.
    ;)
    




(define (eval-expr expr state)
    (if (and (list? expr) (> (length expr) 1)) ;if it is a list
    ;we will go into a switch-like structure.

    (cond   ((eq? (car expr) ':=) 

                ;removing := from exp
                (let ((rem-exp (cdr expr)))
                ;calling the := function I implemented with proper bits of the expression
                (let (( return-state (:= (car rem-exp) (car (cdr rem-exp)) state)))
                (let ((val (get return-state '-r)))
                (printf "value is ~a\n" val)
                (put return-state '-r val))))

            );put returns the newstate   


            ((eq? (car expr) 'if:) 
                (printf "we are in eval-expr and if section \n")
                (let ((rem-exp (cdr expr)))
                (printf "rem-exp is ~a\n" rem-exp)
                (printf "first arg ~a\n" (car rem-exp))
                (printf "second arg ~a\n" (car (cdr rem-exp)))
                (printf "third arg  ~a\n" (car (cdr (cdr rem-exp))))
                (let ((return-state (if: (car rem-exp) (car (cdr rem-exp)) (car (cdr (cdr rem-exp))) state)))

                (let ((val (get return-state '-r)))
                (printf "value is ~a\n" val)
                (put return-state '-r val)))))

    
            ((eq? (car expr) 'while:) 
                (let ((val (eval (cons 'while: expr))))
                (put state '-r val)));put returns the newstate 
            
            ((eq? (car expr) 'func)
                (let ((val (eval (cons 'func: expr))))
                (put state '-r val)));put returns the newstate 

            ;lambda needs rework
            ((eq? (car expr) 'lambda) 
                (let ((val (eval expr)))
                (put state '-r val)));put returns the newstate

            ;symbol needs rework
            ((symbol? (car expr))  ;operation cases
            (printf   "SYMBOL CASE IN EVAL-EXPR AND EXPR IS ~a \n" expr)
            (let ((val  (eval (get (map-eval expr state) '-r))))
            (put state '-r val)));put returns the newstate 

            ;(map-eval expr state) ;map-eval all of the elements in the list and apply the operation to the resulting values.
            ;(put state '-r (eval expr)))  
    )
    
    ;else it is variable or literal
    (cond   ((number? expr) 
            (printf   "NUMBER CASE IN EVAL-EXPR AND EXPR IS ~a \n" expr)
                (let ((val (eval expr)))
                (put state '-r val)));put returns the newstate 

            ((boolean? expr) 
            (printf   "BOOL CASE IN EVAL-EXPR AND EXPR IS ~a \n" expr)
                (let ((val (eval expr)))
                (printf "the value is ~a \n" val)
                (cond   ((eq? val #t)
                        (printf "WE ARE ADDING TRUE TO THE STATE ~a \n" val)
                        (put state '-r '#t))
                        ((eq? val #f)
                        (printf "WE ARE ADDING FALSE  TO THE STATE ~a \n" val)
                        (put state '-r '#f))
                )));put returns the newstate

            ((symbol? expr) 
            (printf   "SINGLE SYMBOL CASE IN EVAL-EXPR AND EXPR IS ~a \n" expr)

            
                (let ((val (get state expr)))
                (put state '-r val)));put returns the newstate 

            ((string? expr) 
            (printf   "STRING CASE IN EVAL-EXPR AND EXPR IS ~a \n" expr)
                (let ((val (eval expr)))
                (put state '-r val)));put returns the newstate 

            ((number? (get state expr)) ;a variable: if the variable with a value is found in the state     
            (printf   "VARIABLE CASE IN EVAL-EXPR AND EXPR IS ~a \n" expr)
                (let ((val (eval (get state expr))))
                (put state '-r val)));put returns the newstate 
    )));


(define (:= var val-expr state)
    (printf "at := ~a ~a ~a\n" var val-expr state)
    (let ((new-state (eval-expr val-expr state)))
    (put new-state var (get new-state '-r))))
    



(define (if: test-expr then-exprs else-exprs state)
    (printf "we are in if: \n")
    (let ((test-state (eval-expr test-expr state)))
    (printf "we are in if: and test-state is ~a \n" test-state) 
    (cond ((eq? (hash-ref test-state '-r) #t)
            (printf "EXPR IS TRUE AND EXPR IS ~a \n" test-expr)
            (eval-exprs then-exprs state))
            (else
            (printf "EXPR IS FALSE AND EXPR IS ~a \n" test-expr)
            (eval-exprs else-exprs state))
            )))


(define (eval-exprs exprs state)
    (foldl eval-expr state exprs))



(define (while: test-expr body-exprs state)
    (let ((test-state (eval-expr test-expr state)))
    (if (hash-ref test-state '-r) ; if -r is true in the resulting state after evaluating test-expr
        (begin ;begin is forbidden
            (display "we are in while: and test-case is true")
            (newline)
            (let ((body-state (eval-exprs body-exprs test-state)))
            (print "we are in while: and evaluated body state is ~a \n" body-state)
            (if (hash-ref body-state '-r) ; if -r is true in the resulting state after evaluating body exprs - repeat
                (while: test-expr body-exprs body-state)
                body-state)))
        state)))


(define (func params body-exprs state)
    (let ((new-state (put state '-r (lambda (args state) (eval-exprs body-exprs (create-hash params args))))))
    new-state))
