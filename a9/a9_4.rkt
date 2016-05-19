#lang racket

(require "parenthec.rkt")
;Transform your continuation constructors to a define-union,
;change the match in apply-k to instead use union-case
; kt_ ;;kt
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
      [(var n) (apply-env env n return)]
      [(mult e0 e1) (value-of-cps e0 env (kt_outer-mult-k e1 env return))]
                   ;(value-of-cps e0 env (λ (n1)
                   ;(value-of-cps e1 env (λ (n2^)
		   ;(apply-k return (* n1 n2^))))))]
      [(sub1 e) (value-of-cps e env (kt_constructor-sub1 return))]
                ;(value-of-cps e env (λ (n)
		;(apply-k return (sub1 n))))]
      [(zero e) (value-of-cps e env (kt_constructor-zero return))]
                ;(value-of-cps e env (λ (n) 
		;(apply-k return (zero? n))))]
      [(if test conseq alt) (value-of-cps test env (kt_constructor-if conseq alt env return))]
      ;[(if e0 e1 e2) (value-of-cps e0 env (λ (b)
                        ;(if b 
                        ;(value-of-cps e1 env return)
                        ;(value-of-cps e2 env return))))]
      [(letcc e) (value-of-cps e (envr_extend-env return env) return)]
      [(throw e0 e1) (value-of-cps e0 env (kt_outer-throw-k e1 env))]
                     ;(value-of-cps e0 env (λ (return)
                     ;(value-of-cps e1 env (λ (v^)
		     ;(apply-k return v^)))))]
      [(let e0 e1) (value-of-cps e0 env (kt_constructor-let e1 env return))]
                   ;(value-of-cps e0 env (λ (v)
		   ;(value-of-cps e1 (envr_extend-env v env) return)))]
      [(lambda e) (apply-k return (clos_closure e env))] 
      [(app rator rand) (value-of-cps rator env (kt_outer-app-k rand env return))])))
      ;[(app e0 e1) (value-of-cps e0 env (λ (clos)
		   ;(value-of-cps e1 env (λ (v^)
		   ;(apply-closure clos v^ return)))))])))

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
    (λ (v) v)))

; apply-k : Cont × Value → Value
;(define apply-k 
 ; (λ (return v)
  ;  (return v)))
(define-union kt
  (empty-k)
  (inner-mult-k v^ return^)
  (outer-mult-k e env return^)
  (constructor-sub1 return^)
  (constructor-zero return^)
  (constructor-if conseq alt env return^)
  (constructor-let body env return^)
  (inner-throw-k e1)
  (outer-throw-k e0 env)
  (inner-app-k v^ return^)
  (outer-app-k rand env return^))
;=====
;inner-mult-k
(define inner-mult-k
  (λ (v^ return^)
    `(inner-mult-k ,v^ ,return^)))
;outer-mult-k
(define outer-mult-k
  (λ (e env return^)
    `(outer-mult-k ,e ,env ,return^)))
;constructor-sub1
(define constructor-sub1
  (λ (return^)
    `(constructor-sub1 ,return^)))
;constructor-zero
(define constructor-zero
  (λ (return^)
    `(constructor-zero ,return^)))
;constructor-if
(define constructor-if
  (λ (conseq alt env return^)
    `(constructor-if ,conseq ,alt ,env ,return^)))
;constructor-let
(define constructor-let
  (λ (body env return^)
    `(constructor-let ,body ,env ,return^)))
;inner-throw-k
(define inner-throw-k
  (λ (e1)
    `(inner-throw-k ,e1)))
;outer-throw-k
(define outer-throw-k
  (λ (e0 env)
    `(outer-throw-k ,e0 ,env)))
;inner-app-k
(define inner-app-k
  (λ (v^ return^)
    `(inner-app-k ,v^ ,return^)))
;outer-app-k
(define outer-app-k
  (λ (rand env return^)
    `(outer-app-k ,rand ,env ,return^)))
        

(define apply-k
  (λ (return v)
    (union-case return kt
      [(empty-k) v]
      [(inner-mult-k v^ return^) (apply-k return^ (* v v^))]
      [(outer-mult-k e env return^) (value-of-cps e env (kt_inner-mult-k v return^))]
      [(constructor-sub1 return^) (apply-k return^ (sub1 v))]
      [(constructor-zero return^) (apply-k return^ (zero? v))]
      [(constructor-if conseq alt env return^) (if v (value-of-cps conseq env return^) (value-of-cps alt env return^))]
      [(constructor-let body env return^) (value-of-cps body (envr_extend-env v env) return^)]
      [(inner-throw-k e1) (apply-k e1 v)]
      [(outer-throw-k e0 env) (value-of-cps e0 env (kt_inner-throw-k v))]
      [(inner-app-k v^ return^) (apply-closure v^ v return^)]
      [(outer-app-k rand env return^) (value-of-cps rand env (kt_inner-app-k v return^))])))
                
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
     ;(envr_empty-env)
     ;(kt_empty-k))))
     (envr_empty-env)
     (kt_empty-k))))

(main)