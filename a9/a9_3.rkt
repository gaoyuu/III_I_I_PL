#lang racket

(require "parenthec.rkt")
;Transform your environment constructors to a define-union
;with envr , envr_
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
      [(var n) (apply-env env n return)];;change lookup to apply-env
      [(mult e0 e1) (value-of-cps e0 env (λ (n1)
                    (value-of-cps e1 env (λ (n2^)
		    (apply-k return (* n1 n2^))))))]
      [(sub1 e) (value-of-cps e env (λ (n)
		(apply-k return (sub1 n))))]
      [(zero e) (value-of-cps e env (λ (n) 
		  (apply-k return (zero? n))))]
      [(if e0 e1 e2) (value-of-cps e0 env (λ (b)
                        (if b 
                            (value-of-cps e1 env return)
                            (value-of-cps e2 env return))))]
      [(letcc e) (value-of-cps e (envr_extend-env return env) return)];;add envr_
      [(throw e0 e1) (value-of-cps e0 env (λ (return)
                     (value-of-cps e1 env (λ (v^)
		     (apply-k return v^)))))]
      [(let e0 e1) (value-of-cps e0 env (λ (v)
		   (value-of-cps e1 (envr_extend-env v env) return)))];;add envr_
      [(lambda e) (apply-k return (clos_closure e env))] 
      [(app e0 e1) (value-of-cps e0 env (λ (clos)
		   (value-of-cps e1 env (λ (v^)
		   (apply-closure clos v^ return)))))])))

; empty-env : () → Env
;;(define empty-env
  ;;(λ ()
    ;;(λ (y return)
      ;;(error 'value-of "unbound identifier ~s" y))))
(define empty-env
  (λ ()
    `(empty-env)))

;;==================untagged-list env
; extend-env : Value × Env → Env
;(define extend-env
;  (λ (v env)
;    (λ (y return)
;      (if (zero? y)
;	  (apply-k return v)
;	  (lookup env (sub1 y) return)))))
;;(define extend-env
  ;;(λ (v env)
    ;;`(extend-env ,v ,env)))

; lookup/apply-env : Env × Var × Cont → Value
;(define lookup 
;  (λ (env y return)
;    (env y return)))
;;(define lookup
  ;;(λ (env y return)
    ;;(match env
      ;;[`(extend-env ,a ,env) (if (zero? y) (apply-k return a) (lookup env (sub1 y) return))]
      ;;[`(empty-env) (error 'value-of "unbound identifier ~s" y)])))
(define-union envr
  (empty-env)
  (extend-env a env))

(define apply-env
  (λ (env y return)
    (union-case env envr
                [(empty-env) (error 'value-of "unbound identifier ~s" y)]
                [(extend-env a env) (if (zero? y)
                                        (apply-k return a)
                                        (apply-env env (sub1 y) return))])));;lookup->apply-env
;;===================
;;===================data-structure representation of closures
; Closure = Value × Env → Value
;(define closure
 ; (λ (e env)
  ;  `(closure ,e ,env)))
(define-union clos
  (closure body env))

; apply-closure : Closure × Value × Cont → Value
;(define apply-closure 
 ; (λ (clos v return)
  ;  (clos v return)))
;(define apply-closure
 ; (λ (clos v return)
  ;  (match clos
   ;   [`(closure ,body ,env) (value-of-cps body (extend-env v env) return)])))
(define apply-closure
  (λ (clo v return)
   (union-case clo clos
     [(closure body env) (value-of-cps body (envr_extend-env v env) return)])));;add envr_

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