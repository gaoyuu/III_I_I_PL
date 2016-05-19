#lang racket

(require "parenthec.rkt")
;Registerize the interpreter. Turn each let* expression to a begin block:
;the former let* bindings will become set! expressions, and the body becomes
;the invocation of a function of no arguments. Change all serious functions
;to be functions of no arguments. Define your global registers using define-registers
;at the top of the program.

;define registers
(define-registers return* v* exp* env* clo* a* y*)

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
  ;(λ (exp env return)
    ;(union-case exp expr
  (λ ()
    (union-case exp* expr
      ;[(const cexp) (let* ([v cexp] [return^ return]) (apply-k return^ v))]
      [(const cexp) (begin (set! return* return*)
                           (set! v* cexp)
                           (apply-k))]
      ;[(var n) (let* ([env^ env] [n^ n] [return^ return]) (apply-env env^ n^ return^))]
      [(var n) (begin (set! y* n)
                      (set! env* env*)
                      (set! return* return*)
                      (apply-env))]
      ;[(mult e0 e1) (let* ([e0^ e0] [env^ env] [return^ (kt_outer-mult-k e1 env^ return)])
      ;                    (value-of-cps e0^ env^ return^))]
      [(mult e0 e1) (begin (set! exp* e0)
                           (set! env* env*)
                           (set! return* (kt_outer-mult-k e1 env* return*))
                           (value-of-cps))]
      ;[(sub1 e) (let* ([e^ e] [env^ env] [return^ (kt_constructor-sub1 return)])
      ;                (value-of-cps e^ env^ return^))]
      [(sub1 e) (begin (set! exp* e)
                       (set! env* env*)
                       (set! return* (kt_constructor-sub1 return*))
                       (value-of-cps))]
      ;[(zero e) (let* ([e^ e] [env^ env] [return^ (kt_constructor-zero return)])
      ;                (value-of-cps e^ env^ return^))]
      [(zero e) (begin (set! exp* e)
                       (set! env* env*)
                       (set! return* (kt_constructor-zero return*))
                       (value-of-cps))]
      ;[(if test conseq alt) (let* ([test^ test] [env^ env] [return^ (kt_constructor-if conseq alt env^ return)])
      ;                            (value-of-cps test^ env^ return^))]
      [(if test conseq alt) (begin (set! exp* test)
                                   (set! env* env*)
                                   (set! return* (kt_constructor-if conseq alt env* return*))
                                   (value-of-cps))]
      ;[(letcc e) (let* ([e^ e] [env^ (envr_extend-env return env)] [return^ return])
      ;                 (value-of-cps e^ env^ return^))]
      [(letcc e) (begin (set! exp* e)
                        (set! env* (envr_extend-env return* env*))
                        (set! return* return*)
                        (value-of-cps))]
      ;[(throw e0 e1) (let* ([e0^ e0] [env^ env] [return^ (kt_outer-throw-k e1 env^)])
      ;                     (value-of-cps e0^ env^ return^))]
      [(throw e0 e1) (begin (set! exp* e0)
                            (set! env* env*)
                            (set! return* (kt_outer-throw-k e1 env*))
                            (value-of-cps))]
      ;[(let e0 e1) (let* ([e0^ e0] [env^ env] [return^ (kt_constructor-let e1 env^ return)])
      ;                   (value-of-cps e0^ env^ return^))]
      [(let e0 e1) (begin (set! exp* e0)
                          (set! env* env*)
                          (set! return* (kt_constructor-let e1 env* return*))
                          (value-of-cps))]
      ;[(lambda e) (let* ([return^ return] [v (clos_closure e env)])
      ;                  (apply-k return^ v))]
      [(lambda e) (begin (set! return* return*)
                         (set! v* (clos_closure e env*))
                         (apply-k))]
      ;[(app rator rand) (let* ([rator^ rator] [env^ env] [return^ (kt_outer-app-k rand env^ return)])
      ;                  (value-of-cps rator^ env^ return^))])))
      [(app rator rand) (begin (set! exp* rator)
                               (set! env* env*)
                               (set! return* (kt_outer-app-k rand env* return*))
                               (value-of-cps))])))
(define empty-env
  (λ ()
    `(empty-env)))

(define-union envr
  (empty-env)
  (extend-env a env))

(define apply-env
  ;(λ (env y return)
    ;(union-case env envr
  (λ ()
    (union-case env* envr
                [(empty-env) (error 'value-of "unbound identifier ~s")]
                [(extend-env a env) (if (zero? y*)
                                        ;(let* ([return^ return] [v a])(apply-k return^ v))
                                        (begin (set! return* return*)
                                               (set! v* a)
                                               (apply-k))
                                        ;(let* ([env^ env] [y^ (sub1 y)] [return^ return])
                                        ;      (apply-env env^ y^ return^)))])))
                                        (begin (set! env* env)
                                               (set! y* (sub1 y*))
                                               (set! return* return*)
                                               (apply-env)))])))
                                        
(define-union clos
  (closure body env))

(define apply-closure
  ;(λ (clo v return)
   ;(union-case clo* clos
  (λ ()
    (union-case clo* clos
     [(closure body env) (begin (set! exp* body)
                                (set! env* (envr_extend-env a* env))
                                (set! return* return*)
                                (value-of-cps))])))
                         ;(let* ([e body] [env (envr_extend-env v env)] [return^ return])
                         ;  (value-of-cps e env return^))])))

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
  ;(λ (return v)
    ;(union-case return kt
  (λ ()
    (union-case return* kt
      ;[(empty-k) v]
      [(empty-k) v*]
      ;[(inner-mult-k v^ return^) (let* ([return^^ return^] [v^^ (* v v^)]) (apply-k return^^ v^^))]
      [(inner-mult-k v^ return^) (begin (set! return* return^)
                                        (set! v* (* v^ v*))
                                        (apply-k))]
      ;[(outer-mult-k e env return^) (let* ([e^ e] [env^ env] [return^^ (kt_inner-mult-k v return^)])(value-of-cps e^ env^ return^^))]
      [(outer-mult-k e env return^) (begin (set! exp* e)
                                           (set! env* env)
                                           (set! return* (kt_inner-mult-k v* return^))
                                           (value-of-cps))]
      ;[(constructor-sub1 return^) (let* ([return^^ return^] [v^ (sub1 v)]) (apply-k return^^ v^))]
      [(constructor-sub1 return^) (begin (set! return* return^)
                                         (set! v* (sub1 v*))
                                         (apply-k))]
      ;[(constructor-zero return^) (let* ([return^^ return^] [v^ (zero? v)]) (apply-k return^^ v^))]
      [(constructor-zero return^) (begin (set! return* return^)
                                         (set! v* (zero? v*))
                                         (apply-k))]
      ;[(constructor-if conseq alt env return^) (if v (let* ([conseq^ conseq] [env^ env] [return^^ return^])
      ;                                               (value-of-cps conseq^ env^ return^^)) 
      ;                                               (let* ([alt^ alt] [env^^ env] [return^^^ return^])
      ;                                               (value-of-cps alt^ env^^ return^^^)))]
      [(constructor-if conseq alt env return^) (if v*
                                                   (begin (set! exp* conseq)
                                                          (set! env* env)
                                                          (set! return* return^)
                                                          (value-of-cps))
                                                   (begin (set! exp* alt)
                                                          (set! env* env)
                                                          (set! return* return^)
                                                          (value-of-cps)))]
      ;[(constructor-let body env return^) (let* ([body^ body] [env^ (envr_extend-env v env)] [return^^ return^]) (value-of-cps body^ env^ return^^))]
      [(constructor-let body env return^) (begin (set! exp* body)
                                                 (set! env* (envr_extend-env v* env))
                                                 (set! return* return^)
                                                 (value-of-cps))]
      ;[(inner-throw-k e1) (let* ([return e1] [v^ v]) (apply-k return v^))]
      [(inner-throw-k e1) (begin (set! return* e1)
                                 (set! v* v*)
                                 (apply-k))]
      ;[(outer-throw-k e0 env) (let* ([e0^ e0] [env^ env] [return (kt_inner-throw-k v)])(value-of-cps e0^ env^ return))]
      [(outer-throw-k e0 env) (begin (set! exp* e0)
                                     (set! env* env)
                                     (set! return* (kt_inner-throw-k v*))
                                     (value-of-cps))]
      ;[(inner-app-k v^ return^) (let* ([clos v^] [v^^ v] [return^^ return^]) (apply-closure clos v^^ return^^))]
      [(inner-app-k v^ return^) (begin (set! clo* v^)
                                       (set! a* v*)
                                       (set! return* return^)
                                       (apply-closure))]
      ;[(outer-app-k rand env return^) (let* ([rand^ rand] [env^ env] [return^^ (kt_inner-app-k v return^)]) (value-of-cps rand^ env^ return^^))])))
      [(outer-app-k rand env return^) (begin (set! exp* rand)
                                             (set! env* env)
                                             (set! return* (kt_inner-app-k v* return^))
                                             (value-of-cps))])))
(define main 
  (λ ()
    ;(value-of-cps
    (begin (set! exp*
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
                   (expr_const 5))))
     ;(envr_empty-env)
     ;(kt_empty-k)
           (set! env* (envr_empty-env))
           (set! return* (kt_empty-k))
           (value-of-cps))))

(main)