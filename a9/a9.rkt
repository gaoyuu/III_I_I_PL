#lang racket

(require "parenthec.rkt")
;Registerize the interpreter. Turn each let* expression to a begin block: the former let* bindings will
;become set! expressions, and the body becomes the invocation of a function of no arguments.
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
      [(const cexp) (let* ([v cexp] [return^ return]) (apply-k return^ v))]
      [(var n) (let* ([env^ env] [n^ n] [return^ return]) (apply-env env^ n^ return^))]
      ;[(mult e0 e1) (value-of-cps e0 env (kt_outer-mult-k e1 env return))]
      [(mult e0 e1) (let* ([e0^ e0] [env^ env] [return^ (kt_outer-mult-k e1 env^ return)])
                          (value-of-cps e0^ env^ return^))]
      ;[(sub1 e) (value-of-cps e env (kt_constructor-sub1 return))]
      [(sub1 e) (let* ([e^ e] [env^ env] [return^ (kt_constructor-sub1 return)])
                      (value-of-cps e^ env^ return^))]
      ;[(zero e) (value-of-cps e env (kt_constructor-zero return))]
      [(zero e) (let* ([e^ e] [env^ env] [return^ (kt_constructor-zero return)])
                      (value-of-cps e^ env^ return^))]
      ;[(if test conseq alt) (value-of-cps test env (kt_constructor-if conseq alt env return))]
      [(if test conseq alt) (let* ([test^ test] [env^ env] [return^ (kt_constructor-if conseq alt env^ return)])
                                  (value-of-cps test^ env^ return^))]
      ;[(letcc e) (value-of-cps e (envr_extend-env return env) return)]
      [(letcc e) (let* ([e^ e] [env^ (envr_extend-env return env)] [return^ return])
                       (value-of-cps e^ env^ return^))]
      ;[(throw e0 e1) (value-of-cps e0 env (kt_outer-throw-k e1 env))]
      [(throw e0 e1) (let* ([e0^ e0] [env^ env] [return^ (kt_outer-throw-k e1 env^)])
                           (value-of-cps e0^ env^ return^))]
      ;[(let e0 e1) (value-of-cps e0 env (kt_constructor-let e1 env return))]
      [(let e0 e1) (let* ([e0^ e0] [env^ env] [return^ (kt_constructor-let e1 env^ return)])
                         (value-of-cps e0^ env^ return^))]
      ;[(lambda e) (apply-k return (clos_closure e env))] 
      [(lambda e) (let* ([return^ return] [v (clos_closure e env)])
                        (apply-k return^ v))]
      ;[(app rator rand) (value-of-cps rator env (kt_outer-app-k rand env return))])))
      [(app rator rand) (let* ([rator^ rator] [env^ env] [return^ (kt_outer-app-k rand env^ return)])
                        (value-of-cps rator^ env^ return^))])))
(define empty-env
  (λ ()
    `(empty-env)))


(define-union envr
  (empty-env)
  (extend-env a env))

(define apply-env
  (λ (env y return)
    (union-case env envr
                [(empty-env) (error 'value-of "unbound identifier ~s" y)]
                [(extend-env a env) (if (zero? y)
                                        (let* ([return^ return] [v a])(apply-k return^ v));modification_5
                                        (let* ([env^ env] [y^ (sub1 y)] [return^ return])
                                              (apply-env env^ y^ return^)))])))

(define-union clos
  (closure body env))

(define apply-closure
  (λ (clo v return)
   (union-case clo clos
     [(closure body env) (let* ([e body] [env (envr_extend-env v env)] [return^ return])
                           (value-of-cps e env return^))])));;modification_5

(define empty-k
  (λ ()
    (λ (v) v)))

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
      ;[(inner-mult-k v^ return^) (apply-k return^ (* v v^))]
      [(inner-mult-k v^ return^) (let* ([return^^ return^] [v^^ (* v v^)]) (apply-k return^^ v^^))]
      ;[(outer-mult-k e env return^) (value-of-cps e env (kt_inner-mult-k v return^))]
      [(outer-mult-k e env return^) (let* ([e^ e] [env^ env] [return^^ (kt_inner-mult-k v return^)])(value-of-cps e^ env^ return^^))]
      ;[(constructor-sub1 return^) (apply-k return^ (sub1 v))]
      [(constructor-sub1 return^) (let* ([return^^ return^] [v^ (sub1 v)]) (apply-k return^^ v^))]
      ;[(constructor-zero return^) (apply-k return^ (zero? v))]
      [(constructor-zero return^) (let* ([return^^ return^] [v^ (zero? v)]) (apply-k return^^ v^))]
      ;[(constructor-if conseq alt env return^) (if v (value-of-cps conseq env return^) (value-of-cps alt env return^))]
      [(constructor-if conseq alt env return^) (if v (let* ([conseq^ conseq] [env^ env] [return^^ return^])
                                                     (value-of-cps conseq^ env^ return^^)) 
                                                     (let* ([alt^ alt] [env^^ env] [return^^^ return^])
                                                     (value-of-cps alt^ env^^ return^^^)))]
      ;[(constructor-let body env return^) (value-of-cps body (envr_extend-env v env) return^)]
      [(constructor-let body env return^) (let* ([body^ body] [env^ (envr_extend-env v env)] [return^^ return^]) (value-of-cps body^ env^ return^^))]
      ;[(inner-throw-k e1) (apply-k e1 v)]
      [(inner-throw-k e1) (let* ([return e1] [v^ v]) (apply-k return v^))]
      ;[(outer-throw-k e0 env) (value-of-cps e0 env (kt_inner-throw-k v))]
      [(outer-throw-k e0 env) (let* ([e0^ e0] [env^ env] [return (kt_inner-throw-k v)])(value-of-cps e0^ env^ return))]
      ;[(inner-app-k v^ return^) (apply-closure v^ v return^)]
      [(inner-app-k v^ return^) (let* ([clos v^] [v^^ v] [return^^ return^]) (apply-closure clos v^^ return^^))]
      ;[(outer-app-k rand env return^) (value-of-cps rand env (kt_inner-app-k v return^))])))
      [(outer-app-k rand env return^) (let* ([rand^ rand] [env^ env] [return^^ (kt_inner-app-k v return^)]) (value-of-cps rand^ env^ return^^))])))

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
     (envr_empty-env)
     (kt_empty-k))))

