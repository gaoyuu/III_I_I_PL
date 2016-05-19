#lang racket

(require "parenthec.rkt")
;Define your program counter at the top of the program.
;(define-program-counter pc*)

;define registers
(define-registers return* v* exp* env-cps* clo* a* y*)
                                  ;env-cps*
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

(define-label value-of-cps
;(define value-of-cps
;  (λ ()
    (union-case exp* expr
      [(const cexp) (begin (set! return* return*)
                           (set! v* cexp)
                           (apply-k))]
      [(var n) (begin (set! y* n)
                      (set! env-cps* env-cps*)
                      (set! return* return*)
                      (apply-env))]
      [(mult e0 e1) (begin (set! exp* e0)
                           (set! env-cps* env-cps*)
                           (set! return* (kt_outer-mult-k e1 env-cps* return*))
                           (value-of-cps))]
      [(sub1 e) (begin (set! exp* e)
                       (set! env-cps* env-cps*)
                       (set! return* (kt_constructor-sub1 return*))
                       (value-of-cps))]
      [(zero e) (begin (set! exp* e)
                       (set! env-cps* env-cps*)
                       (set! return* (kt_constructor-zero return*))
                       (value-of-cps))]
      [(if test conseq alt) (begin (set! exp* test)
                                   (set! env-cps* env-cps*)
                                   (set! return* (kt_constructor-if conseq alt env-cps* return*))
                                   (value-of-cps))]
      [(letcc e) (begin (set! exp* e)
                        (set! env-cps* (envr_extend-env return* env-cps*))
                        (set! return* return*)
                        (value-of-cps))]
      [(throw e0 e1) (begin (set! exp* e0)
                            (set! env-cps* env-cps*)
                            (set! return* (kt_outer-throw-k e1 env-cps*))
                            (value-of-cps))]
      [(let e0 e1) (begin (set! exp* e0)
                          (set! env-cps* env-cps*)
                          (set! return* (kt_constructor-let e1 env-cps* return*))
                          (value-of-cps))]
      [(lambda e) (begin (set! return* return*)
                         (set! v* (clos_closure e env-cps*))
                         (apply-k))]
      [(app rator rand) (begin (set! exp* rator)
                               (set! env-cps* env-cps*)
                               (set! return* (kt_outer-app-k rand env-cps* return*))
                               (value-of-cps))]))
;(define empty-env
;  (λ ()
;    `(empty-env)))

(define-union envr
  (empty-env)
  (extend-env a env-cps))

;(define apply-env
;  (λ ()
(define-label apply-env
  (union-case env-cps* envr
              [(empty-env) (error 'value-of "unbound identifier ~s")]
              [(extend-env a env-cps) (if (zero? y*)
                                      (begin (set! return* return*)
                                             (set! v* a)
                                             (apply-k))
                                      (begin (set! env-cps* env-cps)
                                             (set! y* (sub1 y*))
                                             (set! return* return*)
                                             (apply-env)))]))
                                        
(define-union clos
  (closure body env-cps))

;(define apply-closure
;  (λ ()
(define-label apply-closure
  (union-case clo* clos
     [(closure body env-cps) (begin (set! exp* body)
                                    (set! env-cps* (envr_extend-env a* env-cps))
                                    (set! return* return*)
                                    (value-of-cps))]))

;(define empty-k
;  (λ ()
;    (λ (v) v)))

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
;(define inner-mult-k
;  (λ (v^ return^)
;    `(inner-mult-k ,v^ ,return^)))
;outer-mult-k
;(define outer-mult-k
;  (λ (e env return^)
;    `(outer-mult-k ,e ,env ,return^)))
;constructor-sub1
;(define constructor-sub1
;  (λ (return^)
;    `(constructor-sub1 ,return^)))
;constructor-zero
;(define constructor-zero
;  (λ (return^)
;    `(constructor-zero ,return^)))
;constructor-if
;(define constructor-if
;  (λ (conseq alt env return^)
;    `(constructor-if ,conseq ,alt ,env ,return^)))
;constructor-let
;(define constructor-let
;  (λ (body env return^)
;    `(constructor-let ,body ,env ,return^)))
;inner-throw-k
;(define inner-throw-k
;  (λ (e1)
;    `(inner-throw-k ,e1)))
;outer-throw-k
;(define outer-throw-k
;  (λ (e0 env)
;    `(outer-throw-k ,e0 ,env)))
;inner-app-k
;(define inner-app-k
;  (λ (v^ return^)
;    `(inner-app-k ,v^ ,return^)))
;outer-app-k
;(define outer-app-k
;  (λ (rand env return^)
;    `(outer-app-k ,rand ,env ,return^)))
        
(define-label apply-k
;(define apply-k
;  (λ ()
    (union-case return* kt
      [(empty-k) v*]
      [(inner-mult-k v^ return^) (begin (set! return* return^)
                                        (set! v* (* v^ v*))
                                        (apply-k))]
      [(outer-mult-k e env return^) (begin (set! exp* e)
                                           (set! env-cps* env)
                                           (set! return* (kt_inner-mult-k v* return^))
                                           (value-of-cps))]
      [(constructor-sub1 return^) (begin (set! return* return^)
                                         (set! v* (sub1 v*))
                                         (apply-k))]
      [(constructor-zero return^) (begin (set! return* return^)
                                         (set! v* (zero? v*))
                                         (apply-k))]
      [(constructor-if conseq alt env return^) (if v*
                                                   (begin (set! exp* conseq)
                                                          (set! env-cps* env)
                                                          (set! return* return^)
                                                          (value-of-cps))
                                                   (begin (set! exp* alt)
                                                          (set! env-cps* env)
                                                          (set! return* return^)
                                                          (value-of-cps)))]
      [(constructor-let body env return^) (begin (set! exp* body)
                                                 (set! env-cps* (envr_extend-env v* env))
                                                 (set! return* return^)
                                                 (value-of-cps))]
      [(inner-throw-k e1) (begin (set! return* e1)
                                 (set! v* v*)
                                 (apply-k))]
      [(outer-throw-k e0 env) (begin (set! exp* e0)
                                     (set! env-cps* env)
                                     (set! return* (kt_inner-throw-k v*))
                                     (value-of-cps))]
      [(inner-app-k v^ return^) (begin (set! clo* v^)
                                       (set! a* v*)
                                       (set! return* return^)
                                       (apply-closure))]
      [(outer-app-k rand env return^) (begin (set! exp* rand)
                                             (set! env-cps* env)
                                             (set! return* (kt_inner-app-k v* return^))
                                             (value-of-cps))]))
(define-label main
;(define main 
;  (λ ()
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
           (set! env-cps* (envr_empty-env))
           (set! return* (kt_empty-k))
           (value-of-cps)))

(main)