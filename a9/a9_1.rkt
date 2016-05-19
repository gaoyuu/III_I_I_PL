#lang racket

(require "parenthec.rkt")
;Below #lang racket, add the line (require “parenthec.rkt”). Next add the expr define-union to your file,
;change the match in value-of-cps to use union-case. Make sure to remove the backquotes and commas in the
;patterns of what was your match expression.

;define-union
(define-union expr
  (const cexp)
  (var n)
  (if test conseq alt)
  (mult nexp1 nexp2)
  (sub1 nexp)
  (zero nexp)
  (letcc body)
  (throw kexp vexp)
  (let exp body)              
  (lambda body)
  (app rator rand))

(define value-of-cps
  (λ (exp env return)
    (union-case exp expr
      [(const cexp) (apply-k return cexp)]
      [(var n) (lookup env n return)]
      [(mult e0 e1) (value-of-cps e0 env (λ (n1)
                    (value-of-cps e1 env (λ (n2)
		    (apply-k return (* n1 n2))))))]
      [(sub1 e) (value-of-cps e env (λ (n)
		(apply-k return (sub1 n))))]
      [(zero e) (value-of-cps e env (λ (n) 
		  (apply-k return (zero? n))))]
      [(if e0 e1 e2) (value-of-cps e0 env (λ (b)
                        (if b 
                            (value-of-cps e1 env return)
                            (value-of-cps e2 env return))))]
      [(letcc e) (value-of-cps e (extend-env return env) return)]
      [(throw e0 e1) (value-of-cps e0 env (λ (return)
                     (value-of-cps e1 env (λ (v)
		     (apply-k return v)))))]
      [(let e0 e1) (value-of-cps e0 env (λ (v)
		   (value-of-cps e1 (extend-env v env) return)))]
      [(lambda e) (apply-k return (closure e env))]
      [(app e0 e1) (value-of-cps e0 env (λ (clos)
		   (value-of-cps e1 env (λ (v)
		   (apply-closure clos v return)))))])))


; empty-env : () → Env
(define empty-env
  (λ ()
    (λ (y return)
      (error 'value-of "unbound identifier ~s" y))))


;;==================untagged-list env
; extend-env : Value × Env → Env
;(define extend-env
;  (λ (v env)
;    (λ (y return)
;      (if (zero? y)
;	  (apply-k return v)
;	  (lookup env (sub1 y) return)))))
(define extend-env
  (λ (v env)
    `(extend-env ,v ,env)))

; lookup/apply-env : Env × Var × Cont → Value
;(define lookup 
;  (λ (env y return)
;    (env y return)))
(define lookup
  (λ (env y return)
    (match env
      [`(extend-env ,a ,env) (if (zero? y) (apply-k return a) (lookup env (sub1 y) return))]
      [`(empty-env) (error 'value-of "unbound identifier ~s" y)])))
;;===================
;;===================data-structure representation of closures
; Closure = Value × Env → Value
(define closure
  (λ (e env)
    `(closure ,e ,env)))
  
; apply-closure : Closure × Value × Cont → Value
;(define apply-closure 
 ; (λ (clos v return)
  ;  (clos v return)))
(define apply-closure
  (λ (clos v return)
    (match clos
      [`(closure ,body ,env) (value-of-cps body (extend-env v env) return)])))
;;====================
; empty-k : () → Cont
(define empty-k
  (λ ()
    (λ (v)
      v)))

; apply-k : Cont × Value → Value
(define apply-k 
  (λ (return v)
    (return v)))

(define main 
  (lambda ()
    (value-of-cps 
     (expr_let 
      (expr_lambda
       (expr_lambda 
        (expr_if
         (expr_zero (expr_var 0))
         (expr_const 1)
         (expr_mult (expr_var 0) (expr_app (expr_app (expr_var 1) (expr_var 1)) (expr_sub1 (expr_var 0)))))))
      (expr_mult
       (expr_letcc
        (expr_app
         (expr_app (expr_var 1) (expr_var 1))
         (expr_throw (expr_var 0) (expr_app (expr_app (expr_var 1) (expr_var 1)) (expr_const 4)))))
       (expr_const 5)))
     (empty-env)
     (empty-k))))

(main)