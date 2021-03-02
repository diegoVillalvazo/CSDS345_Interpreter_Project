;Simple Language Interpreter - CSDS 345
;By Robbie Hammond and Diego Villalvazo

#lang racket

(require "simpleParser.rkt")

(define (atom? a)
  (and (not (pair? a)) (not (null? a))))
;essentially a helper function for abstraction
(define interpret
  (lambda (fileName)
    (parser fileName) ))

;actually runs the program and initializes an empty state, works more like a helper function
(define run-program
  (lambda (fileName)
    (interpret-start (interpret fileName) '()) ))

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
      ((eq? (getStmtType stmt) 'var) (M-declare stmt state))
      ((eq? (getStmtType stmt) '=) (M-assign stmt state))
      (else
       (error 'unknown_statement)) )))

;takes a variable name and adds it to the state
(define M-declare
  (lambda (stmt state)
    (cons (cons (getVar stmt) '()) state)))

;takes a statement and a state, evaluates whether or not a variable is declared yet. If it is, it pairs it with the corresponding value
(define M-assign
  (lambda (stmt state)
    (if (declared? (getVar stmt) state) (assign-to (getVar stmt) state (M-value (getVal stmt) state)) (error 'var_not_declared)) ))

;returns the value of a mathematical expression
(define M-value
  (lambda (expr state)
    (cond
      ((number? expr) expr)
      ;((var? expr) (M-var expr state))
      ((eq? (getOp expr) '+) (+ (M-value(getLeftOp expr) state) (M-value(getRightOp expr) state)))
      ((eq? (getOp expr) '-) (- (M-value(getLeftOp expr) state) (M-value(getRightOp expr) state)))
      ((eq? (getOp expr) '*) (* (M-value(getLeftOp expr) state) (M-value(getRightOp expr) state)))
      ((eq? (getOp expr) '/) (quotient (M-value(getLeftOp expr) state) (M-value(getRightOp expr) state)))
      ((eq? (getOp expr) '%) (remainder (M-value(getLeftOp expr) state) (M-value(getRightOp expr) state)))
      ((eq? (getOp expr) '==) (eq? (M-value(getLeftOp expr) state) (M-value(getRightOp expr) state)))
      ((eq? (getOp expr) '!=) (not(eq? (M-value(getLeftOp expr) state) (M-value(getRightOp expr) state))))
      ((eq? (getOp expr) '<) (< (M-value(getLeftOp expr) state) (M-value(getRightOp expr) state)))
      ((eq? (getOp expr) '>) (> (M-value(getLeftOp expr) state) (M-value(getRightOp expr) state)))
      ((eq? (getOp expr) '<=) (<= (M-value(getLeftOp expr) state) (M-value(getRightOp expr) state)))
      ((eq? (getOp expr) '>=) (>= (M-value(getLeftOp expr state)) (M-value(getRightOp expr) state)))
      ((eq? (getOp expr) '&&) (and (M-value(getLeftOp expr) state) (M-value(getRightOp expr) state)))
      ((eq? (getOp expr) '||) (or (M-value(getLeftOp expr) state) (M-value(getRightOp expr) state)))
      ((eq? (getOp expr) '!) (not (M-value(getLeftOp expr) state))) ;does not work with cases such as (! #t)
      (else
       (error 'unknown_operator)) )))

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
      
;returns the first item in a list <-rename, it sounds awful
(define headOf car)

;returns the body of the list <-rename, it sounds awful
(define bodyOf cdr)

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
      ((list? (headOf state)) (or (declared? var (headOf state)) (declared? var (bodyOf state))))
      ((and (eq? (headOf state) var) (not (null? (getVal state)))) #t)
      (else
       (declared? var (bodyOf state))) )))

;returns a state in which a var already declared within a state is paired with the val
(define assign-to
  (lambda (var state val)
    (cond
      ((null? state) state)
      ((list? (headOf state)) (cons (assign-to var (headOf state) val) (assign-to var (bodyOf state) val)))
      ((eq? (headOf state) var) (cons var (cons val '())))
      (else
       (cons (headOf state) (assign-to var (bodyOf state) val))) )))

;returns if something should be considered a var or not
(define var?
  (lambda (expr)
    (if (number? expr) #f #t) ))

;checks whether a var has been declared and if it is, then it returns the value of target var
(define M-var
  (lambda (expr state)
    (if (and (declared? expr state) (initialized? expr state)) (return-var-val expr state) (error 'var_not_initialized)) ))

;returns the variable value from a state
(define return-var-val
  (lambda (var state)
    (cond
      ((null? state) '())
      ((eq? (headOf(headOf state)) var) (bodyOf (headOf state)))
      (else
       (return-var-val var (bodyOf state))) )))

;TESTS

;(M-var 'x '((y)(x)(a)))
;(return-var-val 'z '((x 10)(y 25)(z 5)))

;(initialized? 'x '((a)(b)(x)(d)))

(interpret-start '((var x)(var y)(= x 5)(= y (+ 5 (* 7 5)))) '())

;(run-program "sampleProgram.txt")
;(run-program "doableProgram.txt")
;(interpret-start '((var x)(var y)(var z)(= x 1)(= y (+ 5 (+ 3 2)))(= z x)) '())
