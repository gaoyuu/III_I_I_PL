;;Yu Gao
#lang racket

;;1. Complete the definition of list-ref with a naturally-recursive implementation of nth-cdr
(define list-ref
  (lambda (ls n)
    (letrec 
        ((nth-cdr
          (lambda (n)
            (cond
              [(zero? n) ls]
              [else (cdr (nth-cdr (sub1 n)))]
            ))))
      (car (nth-cdr n)))))

;;2. Define and test a procedure union that takes two lists with no duplicates, 
;;and returns a list containing the union of the two input lists
(define union
  (lambda (ls1 ls2)
    (cond 
      [(null? ls1) ls2]
      [(null? ls2) ls1]
      [(eqv? #f (memv (car ls2) ls1)) (union (append ls1 (list (car ls2))) (cdr ls2))]
      [else (union ls1 (cdr ls2))])))

;;3. Define and test a procedure extend that takes two arguments, say x and pred. 
;;The second argument pred is a predicate. 
(define extend 
  (lambda (x pred)
    (lambda (y)
      (or (eqv? x y) (pred y)))))

;;4. Define and test a procedure walk-symbol that takes a symbol x and an association list s.
(define walk-symbol
  (lambda (x s)
      (cond
        [(not (assv x s)) x]
        [(symbol? (cdr (assv x s)))
         (walk-symbol (cdr (assv x s)) s)]
        [else (cdr (assv x s))])))

;;5. Define and test a procedure lambda->lumbda that takes a lambda-calculus expression and returns the 
;;expression unchanged with the exception that each lambda as a keyword has been replaced with the word lumbda 
(define lambda->lumbda
  (lambda (a)
    (match a
      [(? symbol?) a]
      [`(lambda (,x) ,body) `(lumbda (,x) ,(lambda->lumbda body))]
      [`(,rator ,rand) `(, (lambda->lumbda rator) ,(lambda->lumbda rand))])))

;;6. Define and test a procedure var-occurs? that takes a variable name and a lambda-calculus expression 
;;and returns a boolean answering whether that variable occurs in the expression. 
(define var-occurs?
  (lambda (a expr)
    (match expr
      ((? symbol?) (eqv? a expr))
      (`(lambda (,x) ,body) (var-occurs? a body))
      (`(,rator ,rand) (or (var-occurs? a rator)
                           (var-occurs? a rand))))))
;;7. Define and test a procedure vars that takes a lambda-calculus expression and returns a 
;;list containing all variables that occur in the expression.                         
(define vars
  (lambda (expr)
    (match expr
      ((? symbol?) (list expr))
      (`(lambda (,x) ,body) (vars body))
      (`(,rator ,rand) (append (vars rator)
                               (vars rand))))))
;;8. Define and test a modification of vars called unique-vars that behaves like vars but does not return duplicates. 
(define unique-vars
  (lambda (expr)
    (match expr
      ((? symbol?) (list expr))
      (`(lambda (,x) ,body) (unique-vars body))
      (`(,rator ,rand) (union (unique-vars rator)
                              (unique-vars rand))))))
;;9. Define and test a procedure var-occurs-free? that takes a symbol and a lambda-calculus expression
;;and returns #t if that variable occurs free in that expression, and #f otherwise.
(define var-occurs-free?
  (lambda (sym expr)
    (match expr
      ((? symbol?) (eqv? sym expr))
      (`(lambda (,x) ,body) (if (eqv? x sym) #f (var-occurs-free? sym body)))
      (`(,rator ,rand) (or (var-occurs-free? sym rator)
                           (var-occurs-free? sym rand))))))

;;10. Define and test a procedure var-occurs-bound? that takes a symbol and a lambda-calculus expression 
;;and returns #t if that variable occurs bound in the expression, and #f otherwise. PROBLEM
(define var-occurs-bound?
  (lambda (sym expr)
    (match expr
      ((? symbol?) #f)
      (`(lambda (,x) ,body) (if (eqv? sym x)  
                                (if (memv x (vars body)) 
                                                    #t
                                                    (var-occurs-bound? sym body))
                                (var-occurs-bound? sym body)))
      (`(,rator ,rand) (or (var-occurs-bound? sym rator)
                           (var-occurs-bound? sym rand))))))
;;11. Define and test a procedure unique-free-vars that takes a lambda-calculus expression and returns 
;;a list of all the variables that occur free in that expression. Order doesn't matter, but the list must
;;not contain duplicate variables. 
(define unique-free-vars
  (lambda (expr)
    (match expr
      ((? symbol?) (list expr))
      (`(lambda (,x) ,body) (remv x (unique-free-vars body)))
      (`(,rator ,rand) (union (unique-free-vars rator)
                              (unique-free-vars rand))))))
;;12. Define and test a procedure unique-bound-vars that takes a lambda-calculus expression and 
;;returns a list of all the variables that occur bound in the input expression. 
(define unique-bound-vars 
  (lambda (expr)
    (match expr
      ((? symbol?) '())
      (`(lambda (,x) ,body) (if (memv x (unique-vars body))
                                (cons x (unique-bound-vars body))
                                (unique-bound-vars body)))
      (`(,rator ,rand) (union (unique-bound-vars rator)
                              (unique-bound-vars rand))))))
;;13                  
(define lex
  (lambda (expr ls)
    (match expr
      ((? symbol?) (if (memv expr ls)
                       `(var ,(- (length ls) (length (memv expr  ls))))
                       `(free-var ,expr)))
      (`(lambda (,x) ,body) `(lambda ,(lex body (cons x ls))))
      (`(,rator ,rand) (list (lex rator ls) (lex rand ls))))))