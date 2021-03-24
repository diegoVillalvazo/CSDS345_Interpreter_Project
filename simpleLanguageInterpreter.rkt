;Simple Language Interpreter - CSDS 345
;By Robbie Hammond and Diego Villalvazo

#lang racket

(require "simpleParser.rkt")

;tests if input is an atom
(define (atom? a)
  (and (not (pair? a)) (not (null? a)) (not (boolean? a)))) ;added boolean case

;basically call/cc
(define setFlagFor call/cc)

;just for rn for all the got functions
(define defaultGoto (lambda (v) v))

;get rid of the most recent 
(define popScope
  (lambda (state)
    (cdr state)))

(define inState?
  (lambda (el state)
    (cond
      ((null? state) #f)
      ((eq? (car (car state)) el) #t)
      (else (inState? el (cdr state))))))

(define getLastElement
  (lambda (lis)
    (cond
      ((null? lis) '())
      ((null? (cdr lis)) (car lis))
      (else (getLastElement (cdr lis))))))

; doesn't work, but should update old state with what happened in new state
(define updateState
  (lambda (scopedState fullState)
    (cond
      ((null? scopedState) fullState)
      ((inState? (car (car scopedState)) fullState) (updateState (cdr scopedState) ((M-state fullState)) ));update
      (else (updateState (cdr scopedState) fullState)))))


;tests if the input is the atom 'true or 'false
(define booleanAtom?
  (lambda (a)
    (or (eq? a 'true) (eq? a 'false))))

;converts a booleanAtom to a boolean
(define convertToBoolean
  (lambda (a)
    (cond
      ((eq? a 'true)  #t)
      ((eq? a 'false) #f)
      (else
       (error 'not_a_booleanAtom)) )))

;converts a proper boolean into a booleanAtom
(define convertToProperBoolean
  (lambda (a)
    (cond
      ((eq? a #t) 'true)
      ((eq? a #f) 'false)
      (else
       a) )))

;helper function to start state with 'return
(define initState
  (lambda ()
    '(return) ))

;essentially a helper function for abstraction
(define interpret
  (lambda (fileName)
    (parser fileName) ))

;actually runs the program and initializes an empty state, works more like a helper function
(provide run-program)

(define run-program
  (lambda (fileName)
    (interpret-start (interpret fileName) (initState) defaultGoto defaultGoto defaultGoto) ))

;iterates through the statements in a statement list, takes a state
(define interpret-start
  (lambda (stmt-list state break continue throw)
    (cond
      ((null? stmt-list) state)
      (else
       (interpret-start (bodyOf stmt-list) (M-state (headOf stmt-list) state break continue throw) break continue throw)) )))

;does the all the state manipulation
(define M-state
  (lambda (stmt state break continue throw)
    (display state)
    (display "       ")
    (display stmt)
    (display "\n")
    (cond
      ((eq? (getStmtType stmt) 'var)    (M-declare stmt state break continue throw))
      ((eq? (getStmtType stmt) '=)      (M-assign stmt state break continue throw))
      ((eq? (getStmtType stmt) 'if)     (M-cond-stmt stmt state break continue throw))
      ((eq? (getStmtType stmt) 'while)  (M-while-loop stmt state break continue throw))
      ((eq? (getStmtType stmt) 'return) (M-return stmt state break continue throw))
      ((eq? (getStmtType stmt) 'begin)  (cons (cons (interpret-start (cdr stmt) state break continue throw) '()) state));(M-block (cdr stmt) state (initState)));(interpret-start (cdr stmt) state));(M-block stmt state))
      ((eq? (getStmtType stmt) 'break)  (list (break (cadr stmt))))
      (else
       (display stmt)
       (error 'unknown_statement)) )))

;(define M-block
;  (lambda (stmt state)
;    (cons (car (M-state (cdr stmt) state)) state)
;    (cdr state)))

;(define M-block
;  (lambda (stmt state)
;    (cond
;      ((null? (cdr stmt)) state)
;      (else (cons (M-block (cdr stmt) (M-state (cadr stmt) state)) state))))) ;add the evaluated block statement to the front of state

;(define M-block
;  (lambda (stmt-list oldState newState)
;    (cond
;      ((null? stmt-list) (cons newState oldState))
;      (else
;       (M-block (cdr stmt-list) oldState (M-state (headOf stmt-list) newState))))))

;takes a variable name and adds it to the state
(define M-declare
  (lambda (stmt state break continue throw)
    (cond
      ((eq? (getLength stmt) 2) (cons (cons (getVar stmt) '()) state))
      ((eq? (getLength stmt) 3) (M-declare-assign stmt state break continue throw))
      (else
       (error 'expected_2_or_3_values)))))

;M-declare but it assigns the variable declared as well  :Might condense the line of headOf bodyOf's into something else
(define M-declare-assign
  (lambda (stmt state break continue throw)
    (M-assign (cons '= (cons (getDeclareAssignVar stmt) (cons (getDeclareAssignVal stmt) '()))) (M-declare (cons (getDeclareAssignToDeclareVarType stmt) (cons (getDeclareAssignToDeclareVarName stmt) '())) state break continue throw) break continue throw) )) ;(cons (headOf (bodyOf stmt)) (cons (headOf (bodyOf (bodyOf stmt))) '())))         (M-declare (cons (headOf stmt) (cons (headOf (bodyOf stmt)) '())) state))))

;returns the variable to be used in the declare-assign statement
(define getDeclareAssignVar cadr)

;returns the value to be used in the declare-assign statement
(define getDeclareAssignVal caddr)

;returns the type of the variable to be declared in a declare-assign statement
(define getDeclareAssignToDeclareVarType car)

;returns the name of the variable to be declared ina declare-assign statement
(define getDeclareAssignToDeclareVarName cadr)

;takes a statement and a state, evaluates whether or not a variable is declared yet. If it is, it pairs it with the corresponding value
(define M-assign
  (lambda (stmt state break continue throw)
    (if (declared? (getVar stmt) state) (assign-to (getVar stmt) state (M-value (getVal stmt) state break continue throw)) (error 'var_not_declared)) ))

;helper function that passes appropriate values for condition statement
(define M-cond-stmt
  (lambda (stmt state break continue throw)
    (if (equal? (getStmt1 stmt) (getStmt2 stmt)) (cond-stmt-no-else (getCondition stmt) (getStmt1 stmt) state break continue throw)
    (cond-stmt-with-else (getCondition stmt) (getStmt1 stmt) (getStmt2 stmt) state))))

;helper function that takes the while loop statement and calls on the main function with the right inputs
(define M-while-loop
  (lambda (stmt state break continue throw)
    (while (getCondition stmt) (getWhileBody stmt) state break continue throw)))

;returns a statement
(define M-return
  (lambda (stmt state break continue throw)
    (convertToProperBoolean(M-value stmt state break continue throw))))

;returns the value of a mathematical expression
(define M-value
  (lambda (expr state break continue throw)
    (cond
      ((number? expr) expr)
      ((booleanAtom? expr) (convertToBoolean expr))
      ((var? expr) (M-var expr state)) ;<--- should take care of all variables
      ((eq? (getOp expr) '+)      (+          (M-value(getLeftOp expr) state break continue throw) (M-value(getRightOp expr) state break continue throw)))
      ((eq? (getOp expr) '-)      (checkMinusSignUsage expr state break continue throw))
      ((eq? (getOp expr) '*)      (*          (M-value(getLeftOp expr) state break continue throw) (M-value(getRightOp expr) state break continue throw)))
      ((eq? (getOp expr) '/)      (quotient   (M-value(getLeftOp expr) state break continue throw) (M-value(getRightOp expr) state break continue throw)))
      ((eq? (getOp expr) '%)      (remainder  (M-value(getLeftOp expr) state break continue throw) (M-value(getRightOp expr) state break continue throw)))
      ((eq? (getOp expr) '==)     (eq?        (M-value(getLeftOp expr) state break continue throw) (M-value(getRightOp expr) state break continue throw)))
      ((eq? (getOp expr) '!=)     (not(eq?    (M-value(getLeftOp expr) state break continue throw) (M-value(getRightOp expr) state break continue throw))))
      ((eq? (getOp expr) '<)      (<          (M-value(getLeftOp expr) state break continue throw) (M-value(getRightOp expr) state break continue throw)))
      ((eq? (getOp expr) '>)      (>          (M-value(getLeftOp expr) state break continue throw) (M-value(getRightOp expr) state break continue throw)))
      ((eq? (getOp expr) '<=)     (<=         (M-value(getLeftOp expr) state break continue throw) (M-value(getRightOp expr) state break continue throw)))
      ((eq? (getOp expr) '>=)     (>=         (M-value(getLeftOp expr) state break continue throw) (M-value(getRightOp expr) state break continue throw)))
      ((eq? (getOp expr) '&&)     (and        (M-value(getLeftOp expr) state break continue throw) (M-value(getRightOp expr) state break continue throw)))
      ((eq? (getOp expr) '||)     (or         (M-value(getLeftOp expr) state break continue throw) (M-value(getRightOp expr) state break continue throw)))
      ((eq? (getOp expr) '!)      (not        (M-value(getLeftOp expr) state break continue throw))) ;does not work with cases such as (! #t)
      ((eq? (getOp expr) 'return) (M-value(getLeftOp expr) state break continue throw))
      (else
       (error 'unknown_operator)) )))

;checks if the '- is either unary or binary
(define checkMinusSignUsage
  (lambda (expr state)
    (if (eq? (getLength expr) 2) (* -1 (M-value(getLeftOp expr) state)) (- (M-value(getLeftOp expr) state) (M-value(getRightOp expr) state))) ))
       
;return the operator
(define getOp car)

;return the left operand
(define getLeftOp cadr)

;return the right operand
(define getRightOp caddr)

;returns the first sign in a statement
(define getStmtType car)
  
;return var
(define getVar cadr)

;return value
(define getVal caddr)

;return getLinkedVal
(define getLinkedVal cdr)

;returns the first item in a list
(define headOf car)

;returns the body of the list
(define bodyOf cdr)

;returns condition
(define getCondition cadr)

;returns result for if
(define getStmt1 caddr)


;returns result for then
(define getStmt2
  (lambda (stmt)
    (cond
      ((eq? (getLength stmt) 3) (getStmt1 stmt))
      ((eq? (getStmtType stmt) 'if) (cadddr stmt)) ;if the second statement is another conditional statement, return the other conditoinal statement
      (else (cadddr stmt))))) ;otherwise it isn't so just return the statement
     
;returns the length of a statement
(define getLength
  (lambda (stmt)
    (if (null? stmt) 0 (+ 1 (getLength (bodyOf stmt))))))

;returns the loop body of the while statement
(define getWhileBody caddr);cdaddr);caddr)

;returns whether var has been declared by checking if it is in the state
(define declared?
  (lambda (var state)
    (cond
      ((null? state) #f)
      ((list? (headOf state)) (or (declared? var (headOf state)) (declared? var (bodyOf state))))
      ((eq? (headOf state) var) #t)
      (else
       (declared? var (bodyOf state))))))

;returns whether var has been initialized by checking if it is in the state AND has a value
(define initialized?
  (lambda (var state)
    (cond
      ((null? state) #f)
      ((list? (headOf state)) (or (initialized? var (headOf state)) (initialized? var (bodyOf state))))
      ((and (eq? (headOf state) var) (not (null? (getLinkedVal state))))  #t)
      (else
       (initialized? var (bodyOf state))) )))

;returns a state in which a var already declared within a state is paired with the val
(define assign-to
  (lambda (var state val)
    (cond
      ((null? state) state)
      ((list? (headOf state)) (cons (assign-to var (headOf state) val) (assign-to var (bodyOf state) val)))
      ((eq? (headOf state) var) (cons var (cons val '())))
      (else
       (cons (headOf state) (assign-to var (bodyOf state) val))) )))

;returns if something should be considered a var or not essentially just an atom
(define var?
  (lambda (expr)
    (if (atom? expr) #t #f) ))

;checks whether a var has been declared and if it is, then it returns the value of target var
(define M-var
  (lambda (expr state)
    (if (and (declared? expr state) (initialized? expr state)) (return-var-val expr state) (error 'var_not_initialized)) ))

;returns the variable value from a state
(define return-var-val
  (lambda (var state)
    (cond
      ((null? state) (error 'no_value_assigned))
      ((list? (headOf (headOf state))) (return-var-val var (headOf state)))
      ((eq? (headOf(headOf state)) var) (headOf (bodyOf (headOf state))))
      (else
       (return-var-val var (bodyOf state))) )))

;Evaluates an if-else statement in the form of "if condition, then stmt1. Else, stmt2"
(define cond-stmt-with-else
  (lambda (condition stmt1 stmt2 state break continue throw)
    (if (M-value condition state) (M-state stmt1 state break continue throw
                                           ) (M-state stmt2 state break continue throw))))

;evaluates an if statement (without else) in the form of "if condition, then stmt1."
(define cond-stmt-no-else
  (lambda (condition stmt1 state break continue throw)
    (if (M-value condition state break continue throw) (M-state stmt1 state break continue throw) state)))

;while loop statement in the form of "while condition stmt"
(define while
  (lambda (condition loopbody state break continue throw)
    (if (M-value condition state break continue throw) (while condition loopbody (M-state loopbody state break continue throw) break continue throw) state)))

;(inState? 'x '((x 5) (y 10)))
;(updateState '((x 10) (y 5)) '((x 5) (y 3)))
;(display "\n\n\n")
;(interpret "tests_old/test19.txt")
;(run-program "tests/test1.txt")
;(interpret-start '((var x 10) (begin (var y) (= y 6) (var z 5)) (return z)) (initState))
;(interpret-start '((var x 0) (var y 10) (while (> y x) (begin (var a 20) (= x (+ a a)))) (return x)) (initState))
;(interpret-start '((var x 10) (begin (var y) (= y 6) (break 5) (var z 5)) (return z)) (initState) defaultGoto defaultGoto defaultGoto)
(run-program "tests/test2.txt")
