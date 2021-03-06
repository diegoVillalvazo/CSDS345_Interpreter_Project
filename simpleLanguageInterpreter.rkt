;Simple Language Interpreter - CSDS 345
;By Robbie Hammond and Diego Villalvazo

#lang racket

(require "simpleParser.rkt")

;tests if input is an atom
(define (atom? a)
  (and (not (pair? a)) (not (null? a)) (not (boolean? a)))) ;added boolean case

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
    '((return)) ))

;essentially a helper function for abstraction
(define interpret
  (lambda (fileName)
    (parser fileName) ))

;actually runs the program and initializes an empty state, works more like a helper function
(provide run-program)
(define run-program
  (lambda (fileName)
    (interpret-start (interpret fileName) (initState)) ))

;iterates through the statements in a statement list, takes a state
(define interpret-start
  (lambda (stmt-list state)
    (cond
      ((null? stmt-list) state)
      (else
       (interpret-start (bodyOf stmt-list) (M-state (headOf stmt-list) state))) )))

;does the all the state manipulation
(define M-state
  (lambda (stmt state)
    (cond
      ((eq? (getStmtType stmt) 'var)    (M-declare stmt state))
      ((eq? (getStmtType stmt) '=)      (M-assign stmt state))
      ((eq? (getStmtType stmt) 'if)     (M-cond-stmt stmt state))
      ((eq? (getStmtType stmt) 'while)  (M-while-loop stmt state))
      ((eq? (getStmtType stmt) 'return) (M-return stmt state))
      (else
       (error 'unknown_statement)) )))

;takes a variable name and adds it to the state
(define M-declare
  (lambda (stmt state)
    (cond
      ((eq? (getLength stmt) 2) (cons (cons (getVar stmt) '()) state))
      ((eq? (getLength stmt) 3) (M-declare-assign stmt state))
      (else
       (error 'expected_2_or_3_values)))))

;M-declare but it assigns the variable declared as well  :Might condense the line of headOf bodyOf's into something else
(define M-declare-assign
  (lambda (stmt state)
    (M-assign (cons '= (cons (getDeclareAssignVar stmt) (cons (getDeclareAssignVal stmt) '()))) (M-declare (cons (getDeclareAssignToDeclareVarType stmt) (cons (getDeclareAssignToDeclareVarName stmt) '())) state)))) ;(cons (headOf (bodyOf stmt)) (cons (headOf (bodyOf (bodyOf stmt))) '())))         (M-declare (cons (headOf stmt) (cons (headOf (bodyOf stmt)) '())) state))))

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
  (lambda (stmt state)
    (if (declared? (getVar stmt) state) (assign-to (getVar stmt) state (M-value (getVal stmt) state)) (error 'var_not_declared)) ))

;helper function that passes appropriate values for condition statement
(define M-cond-stmt
  (lambda (stmt state)
    (if (equal? (getStmt1 stmt) (getStmt2 stmt)) (cond-stmt-no-else (getCondition stmt) (getStmt1 stmt) state)
    (cond-stmt-with-else (getCondition stmt) (getStmt1 stmt) (getStmt2 stmt) state))))

;helper function that takes the while loop statement and calls on the main function with the right inputs
(define M-while-loop
  (lambda (stmt state)
    (while (getCondition stmt) (getWhileBody stmt) state)))

;returns a statement
(define M-return
  (lambda (stmt state)
    (convertToProperBoolean(M-value stmt state))))

;returns the value of a mathematical expression
(define M-value
  (lambda (expr state)
    (cond
      ((number? expr) expr)
      ((booleanAtom? expr) (convertToBoolean expr))
      ((var? expr) (M-var expr state)) ;<--- should take care of all variables
      ((eq? (getOp expr) '+)      (+          (M-value(getLeftOp expr) state) (M-value(getRightOp expr) state)))
      ((eq? (getOp expr) '-)      (checkMinusSignUsage expr state))
      ((eq? (getOp expr) '*)      (*          (M-value(getLeftOp expr) state) (M-value(getRightOp expr) state)))
      ((eq? (getOp expr) '/)      (quotient   (M-value(getLeftOp expr) state) (M-value(getRightOp expr) state)))
      ((eq? (getOp expr) '%)      (remainder  (M-value(getLeftOp expr) state) (M-value(getRightOp expr) state)))
      ((eq? (getOp expr) '==)     (eq?        (M-value(getLeftOp expr) state) (M-value(getRightOp expr) state)))
      ((eq? (getOp expr) '!=)     (not(eq?    (M-value(getLeftOp expr) state) (M-value(getRightOp expr) state))))
      ((eq? (getOp expr) '<)      (<          (M-value(getLeftOp expr) state) (M-value(getRightOp expr) state)))
      ((eq? (getOp expr) '>)      (>          (M-value(getLeftOp expr) state) (M-value(getRightOp expr) state)))
      ((eq? (getOp expr) '<=)     (<=         (M-value(getLeftOp expr) state) (M-value(getRightOp expr) state)))
      ((eq? (getOp expr) '>=)     (>=         (M-value(getLeftOp expr) state) (M-value(getRightOp expr) state)))
      ((eq? (getOp expr) '&&)     (and        (M-value(getLeftOp expr) state) (M-value(getRightOp expr) state)))
      ((eq? (getOp expr) '||)     (or         (M-value(getLeftOp expr) state) (M-value(getRightOp expr) state)))
      ((eq? (getOp expr) '!)      (not        (M-value(getLeftOp expr) state))) ;does not work with cases such as (! #t)
      ((eq? (getOp expr) 'return) (M-value(getLeftOp expr) state))
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
(define getWhileBody caddr)

;returns whether var has been declared by checking if it is in the state
(define declared?
  (lambda (var state)
    (cond
      ((null? state) #f)
      ((list? (headOf state)) (or (declared? var (headOf state)) (declared? var (bodyOf state))))
      ((eq? (headOf state) var) #t)
      (else
       (declared? var (bodyOf state))) )))

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
      ((eq? (headOf(headOf state)) var) (headOf (bodyOf (headOf state))))
      (else
       (return-var-val var (bodyOf state))) )))

;Evaluates an if-else statement in the form of "if condition, then stmt1. Else, stmt2"
(define cond-stmt-with-else
  (lambda (condition stmt1 stmt2 state)
    (if (M-value condition state) (M-state stmt1 state) (M-state stmt2 state))))

;evaluates an if statement (without else) in the form of "if condition, then stmt1."
(define cond-stmt-no-else
  (lambda (condition stmt1 state)
    (if (M-value condition state) (M-state stmt1 state) state)))

;while loop statement in the form of "while condition stmt"
(define while
  (lambda (condition loopbody state)
    (if (M-value condition state) (while condition loopbody (M-state loopbody state)) state)))
