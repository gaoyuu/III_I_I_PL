#lang racket

(let ([f (case-lambda
            [() 10]
            [(x) x]
            [(x y) (list y x)]
            [r r])])
    (list (f)
          (f 1)
          (f 1 2)
          (f 1 2 3)))

(match-let ([(list a b) '(1 2)]
              [(vector x ...) #(1 2 3 4)])
    (list b a x))

; '(10 1 (2 1) (1 2 3))
; '(2 1 (1 2 3 4))

; value-of : Exp x Env -> Value
(define value-of
  (lambda (exp env)
    (match exp
      ;[`,x #:when (symbol? x) (env x)]
      [`,x #:when (symbol? x) (apply-env env x)]
      [`,n #:when (number? n) n]
      [`,b #:when (boolean? b) b]
      [`(if ,pred ,conseq ,altern) (if (value-of pred env)
                                       (value-of conseq env)
                                       (value-of altern env))]
      [`(sub1 ,n) (sub1 (value-of n env))]
      [`(* ,m ,n) (* (value-of m env) (value-of n env))]
      
      ;[`(let ([,x ,e]) ,body) (value-of body (lambda (y) (if (eqv? x y) (value-of e env) (env y))))]
      [`(let ([,x ,e]) ,body) (value-of body (extend-env x (value-of e env) env))]
      
      ;[`(lambda (,x) ,body) (lambda (a) (value-of body (lambda (y) (if (eqv? x y) a (env y)))))]
      [`(lambda (,x) ,body) (lambda (a) (value-of body (extend-env x a env)))]
      
      [`(,rator ,rand) ((value-of rator env) (value-of rand env)) ]
      )))

(define value-of-dy
  (lambda (exp env)
    (match exp
      ;[`,x #:when (symbol? x) (env x)]
      [`,x #:when (symbol? x) (apply-env env x)]
      [`,n #:when (number? n) n]
      [`,b #:when (boolean? b) b]
      [`(if ,pred ,conseq ,altern) (if (value-of pred env)
                                       (value-of conseq env)
                                       (value-of altern env))]
      [`(sub1 ,n) (sub1 (value-of n env))]
      [`(* ,m ,n) (* (value-of m env) (value-of n env))]
      
      ;[`(let ([,x ,e]) ,body) (value-of body (lambda (y) (if (eqv? x y) (value-of e env) (env y))))]
      [`(let ([,x ,e]) ,body) (value-of body (extend-env x (value-of e env) env))]
      
      ;[`(lambda (,x) ,body) (lambda (a) (value-of body (lambda (y) (if (eqv? x y) a (env y)))))]
      [`(lambda (,x) ,body) (lambda (a env) (value-of body (extend-env x a env)))]
      
      [`(,rator ,rand) ((value-of rator env) (value-of rand env) env) ]
      )))

; Env = Var -> Value

; empty-env : () -> Env
;(define empty-env
;  (lambda ()
;    (lambda (y)
;      y)))

; extend-env : Var x Value x Env -> Env
;(define extend-env
;  (lambda (x a env)
;    (lambda (y)
;      (if (eqv? x y)
;          a
;          (apply-env env y)))))

; apply-env : Env x Var -> Value
;(define apply-env
;  (lambda (env y)
;    (env y)))

; Env ::= (empty-env)
;       | (extend-env Var Value Env)

; empty-env : () -> Env
(define empty-env
  (lambda ()
    '(empty-env)))

; extend-env : Var x Value x Env -> Env
(define extend-env
  (lambda (x a env)
    `(extend-env ,x ,a ,env)))

; apply-env : Env x Var -> Value
(define apply-env 
  (lambda (env y)
    (match env
      [`(empty-env) y]
      [`(extend-env ,x ,a ,env) (if (eqv? x y) a (apply-env env y))])))



     