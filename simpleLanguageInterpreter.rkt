;Simple Language Interpreter - CSDS 345
;By Robbie Hammond and Diego Villalvazo

#lang racket

(require "simpleParser.rkt")

;vvv MAIN COMPILER vvv

;tests if input is an atom
(define (atom? a)
  (and (not (pair? a)) (not (null? a)) (not (boolean? a)))) ;added boolean case

;get rid of the most recent 
(define popScope
  (lambda (state)
    (if (number? (car state)) state (cdr state))))

;push in a scope
(define pushScope
  (lambda (scope state)
      (cons scope state)))

;tests whether a variable is in a given state
(define inState?
  (lambda (el state)
    (cond
      ((null? state) #f)
      ((eq? (car (car state)) el) #t)
      (else (inState? el (cdr state))))))

;returns the last element in a list
(define getLastElement
  (lambda (lis)
    (cond
      ((null? lis) '())
      ((null? (cdr lis)) (car lis))
      (else (getLastElement (cdr lis))))))

;updates the old state with what happened in new state
(define updateState
  (lambda (scopedState fullState)
    (cond
      ((null? scopedState) fullState)
      ((number? scopedState) scopedState)
      ((inState? (car (car scopedState)) fullState) (updateState (cdr scopedState) (transferToState (car scopedState) fullState)));((M-state fullState defaultGoto defaultGoto defaultGoto defaultGoto)) ));update
      (else (updateState (cdr scopedState) fullState)))))

;essentially takes a value, say (x 20) and converts it to a statement such as (= x 20) and passes that as a statement on the state
(define transferToState
  (lambda (scoped full)
    (M-state (cons '= (cons (car scoped) (cons (cadr scoped) '()))) full defaultGoto defaultGoto defaultGoto defaultGoto)))

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

;helper function to start state
(define initState '() )

;essentially a helper function for abstraction
(define interpret
  (lambda (fileName)
    (parser fileName) ))

;actually runs the program and initializes an empty state, works more like a helper function
(provide run-program)

(define run-program
  (lambda (fileName)
    (interpret-start (interpret fileName))));(interpret fileName) initState defaultGoto defaultGoto defaultGoto defaultGoto) ))

(define interpret-start
  (lambda (stmt-list)
    (setFlagFor (lambda (return) (interpret-loop stmt-list initState return defaultGoto defaultGoto defaultGoto)))))

;iterates through the statements in a statement list, takes a state
(define interpret-loop
  (lambda (stmt-list state return break continue throw)
    (cond
      ((null? stmt-list) state)
      ((atom? state) state)
      (else
       (interpret-loop (bodyOf stmt-list) (M-state (headOf stmt-list) state return break continue throw) return break continue throw)) )))

;does the all the state manipulation
(define M-state
  (lambda (stmt state return break continue throw)
    (display state)
    (display "       ")
    (display stmt)
    (display "\n")
    (cond
      ((var? stmt)                      (M-value stmt state return break continue throw))
      ((eq? (getStmtType stmt) 'var)    (M-declare stmt state return break continue throw))
      ((eq? (getStmtType stmt) '=)      (M-assign stmt state return break continue throw))
      ((eq? (getStmtType stmt) 'if)     (M-cond-stmt stmt state return break continue throw))
      ((eq? (getStmtType stmt) 'while)  (M-while-loop stmt state return break continue throw))
      ((eq? (getStmtType stmt) 'begin)  (M-block stmt state return break continue throw))
      ((eq? (getStmtType stmt) 'return) (return (M-state (cadr stmt) state return break continue throw)));(return (M-return stmt state return break continue throw)))
      ((eq? (getStmtType stmt) 'continue) (continue state))
      ((eq? (getStmtType stmt) 'break)  (list (break (cadr stmt))))
      (else
       (display stmt)
       (error 'unknown_statement)) )))

;basically "catches" the block of code and starts interpretation the same way as interpret-start does
(define M-block
  (lambda (stmt state return break continue throw)
    (updateState (car (pushScope (M-block-interpret (bodyOf stmt) (returnUpperScope state) return break continue throw) state)) state)))

;interpret the actual code in the block
(define M-block-interpret
  (lambda (stmt state return break continue throw)
    (cond
      ((null? stmt) state);(display state))
      ((number? state) state)
      (else (M-block-interpret (bodyOf stmt) (M-state (headOf stmt) state return break continue throw) return break continue throw)))))

;returns the "upper" scope of a state - i.e. the variables that are above the current scope
(define returnUpperScope
  (lambda (state)
    (cond
      ((null? state) '())
      ((number? state))
      ((list? (headOf (headOf state))) (returnUpperScope (bodyOf state)))
      (else
       (cons (headOf state) (returnUpperScope (bodyOf state)))))))

;takes a variable name and adds it to the state
(define M-declare
  (lambda (stmt state return break continue throw)
    (cond
      ((eq? (getLength stmt) 2) (cons (cons (getVar stmt) '()) state))
      ((eq? (getLength stmt) 3) (M-declare-assign stmt state return break continue throw))
      (else
       (error 'expected_2_or_3_values)))))

;M-declare but it assigns the variable declared as well  :Might condense the line of headOf bodyOf's into something else
(define M-declare-assign
  (lambda (stmt state return break continue throw)
    (M-assign (cons '= (cons (getDeclareAssignVar stmt) (cons (getDeclareAssignVal stmt) '()))) (M-declare (cons (getDeclareAssignToDeclareVarType stmt) (cons (getDeclareAssignToDeclareVarName stmt) '())) state return break continue throw) return break continue throw) )) ;(cons (headOf (bodyOf stmt)) (cons (headOf (bodyOf (bodyOf stmt))) '())))         (M-declare (cons (headOf stmt) (cons (headOf (bodyOf stmt)) '())) state))))

;takes a statement and a state, evaluates whether or not a variable is declared yet. If it is, it pairs it with the corresponding value
(define M-assign
  (lambda (stmt state return break continue throw)
    (if (declared? (getVar stmt) state) (assign-to (getVar stmt) state (M-value (getVal stmt) state return break continue throw)) (error 'var_not_declared)) ))

;helper function that passes appropriate values for condition statement
(define M-cond-stmt
  (lambda (stmt state return break continue throw)
    (if (equal? (getStmt1 stmt) (getStmt2 stmt)) (cond-stmt-no-else (getCondition stmt) (getStmt1 stmt) state return break continue throw)
    (cond-stmt-with-else (getCondition stmt) (getStmt1 stmt) (getStmt2 stmt) state return break continue throw))))

;helper function that takes the while loop statement and calls on the main function with the right inputs
(define M-while-loop
  (lambda (stmt state return break continue throw)
    (while (getCondition stmt) (getWhileBody stmt) state return break continue throw)))

;returns a statement
(define M-return
  (lambda (stmt state return break continue throw)
    (convertToProperBoolean(M-value stmt state return break continue throw))))

;returns the value of a mathematical expression
(define M-value
  (lambda (expr state return break continue throw)
    (cond
      ((number? state) state)
      ((number? expr) expr)
      ((booleanAtom? expr) (convertToBoolean expr))
      ((var? expr) (M-var expr state)) ;<--- should take care of all variables
      ((eq? (getOp expr) '+)      (+          (M-value(getLeftOp expr) state return break continue throw) (M-value(getRightOp expr) state return break continue throw)))
      ((eq? (getOp expr) '-)      (checkMinusSignUsage expr state return break continue throw))
      ((eq? (getOp expr) '*)      (*          (M-value(getLeftOp expr) state return break continue throw) (M-value(getRightOp expr) state return break continue throw)))
      ((eq? (getOp expr) '/)      (quotient   (M-value(getLeftOp expr) state return break continue throw) (M-value(getRightOp expr) state return break continue throw)))
      ((eq? (getOp expr) '%)      (remainder  (M-value(getLeftOp expr) state return break continue throw) (M-value(getRightOp expr) state return break continue throw)))
      ((eq? (getOp expr) '==)     (eq?        (M-value(getLeftOp expr) state return break continue throw) (M-value(getRightOp expr) state return break continue throw)))
      ((eq? (getOp expr) '!=)     (not(eq?    (M-value(getLeftOp expr) state return break continue throw) (M-value(getRightOp expr) state return break continue throw))))
      ((eq? (getOp expr) '<)      (<          (M-value(getLeftOp expr) state return break continue throw) (M-value(getRightOp expr) state return break continue throw)))
      ((eq? (getOp expr) '>)      (>          (M-value(getLeftOp expr) state return break continue throw) (M-value(getRightOp expr) state return break continue throw)))
      ((eq? (getOp expr) '<=)     (<=         (M-value(getLeftOp expr) state return break continue throw) (M-value(getRightOp expr) state return break continue throw)))
      ((eq? (getOp expr) '>=)     (>=         (M-value(getLeftOp expr) state return break continue throw) (M-value(getRightOp expr) state return break continue throw)))
      ((eq? (getOp expr) '&&)     (and        (M-value(getLeftOp expr) state return break continue throw) (M-value(getRightOp expr) state return break continue throw)))
      ((eq? (getOp expr) '||)     (or         (M-value(getLeftOp expr) state return break continue throw) (M-value(getRightOp expr) state return break continue throw)))
      ((eq? (getOp expr) '!)      (not        (M-value(getLeftOp expr) state return break continue throw))) ;does not work with cases such as (! #t)
      ;((eq? (getOp expr) 'return) (return (M-value(getLeftOp expr) state return break continue throw))) <- Not necessary
      (else
       (display "Unknown operator: ")(display (getOp expr))(display "\n")
       (display "Current state: ")(display state)(display "\n")
       (display "Current expression: ")(display expr)(display "\n")
       (error 'unknown_operator)) )))

;checks if the '- is either unary or binary
(define checkMinusSignUsage
  (lambda (expr state return break continue throw)
    (if (eq? (getLength expr) 2) (* -1 (M-value(getLeftOp expr) state return break continue throw)) (- (M-value(getLeftOp expr) state return break continue throw) (M-value(getRightOp expr) state return break continue throw))) ))

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

;returns whether var has been declared by checking if it is in the state
(define declared?
  (lambda (var state)
    ;(display "\n***CURRENT STATE***: ")
    ;(display state)
    ;(display "\n")
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
  (lambda (condition stmt1 stmt2 state return break continue throw)
    (if (M-value condition state return break continue throw) (M-state stmt1 state return break continue throw
                                           ) (M-state stmt2 state return break continue throw))))

;evaluates an if statement (without else) in the form of "if condition, then stmt1."
(define cond-stmt-no-else
  (lambda (condition stmt1 state return break continue throw)
    (if (M-value condition state return break continue throw) (M-state stmt1 state return break continue throw) state)))

;while loop statement in the form of "while condition stmt"
(define while
  (lambda (condition loopbody state return break continue throw)
    (display loopbody)
    (if (M-value condition state return break continue throw) (while condition loopbody (M-state condition loopbody state return break (setFlagFor (lambda (s) (while condition loopbody s return break continue throw))) throw) return break continue throw) (continue state))))

(define loop
  (lambda (condition loopbody state return break continue throw)
    (if (M-value condition state return break continue throw)
    (M-state loopbody state break (call/cc (lambda (s) (while condition loopbody s return break continue throw))) throw) (continue state))))

;vvv DEFINITIONS AND OTHER STUFF vvv

;basically call/cc
(define setFlagFor call/cc)

;just for rn for all the got functions
(define defaultGoto (lambda (v) v))

;returns the statement list for a block
(define getStmtList cdr)

;returns the variable to be used in the declare-assign statement
(define getDeclareAssignVar cadr)

;returns the value to be used in the declare-assign statement
(define getDeclareAssignVal caddr)

;returns the type of the variable to be declared in a declare-assign statement
(define getDeclareAssignToDeclareVarType car)

;returns the name of the variable to be declared ina declare-assign statement
(define getDeclareAssignToDeclareVarName cadr)

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

;returns the loop body of the while statement
(define getWhileBody caddr);cdaddr);caddr)

(run-program "tests/test8.txt")


;vvv TESTS vvv
;(interpret "tests/test7.txt")
;(run-program "tests/test7.txt") ;<---- doesnt work
;(interpret-start '((var x 10)(var y 2)(begin (var a 9000)(var b 8000)(= x (+ x b)))(return x)) initState defaultGoto defaultGoto defaultGoto defaultGoto)
;(interpret-start '((var x 0) (var result 0) (while (< x 10) (begin (if (> result 15) (begin (return result))) (= result (+ result x)) (= x (+ x 1)))) (return result)))
;(interpret-start '((var x 10)(var a 0) (if (== 10 x) (begin (= a (+ a x)) (return a)) (begin (var y (* 2 x)) (return y)))))
;(interpret-start '((var x)(= x 10)(return x)))



