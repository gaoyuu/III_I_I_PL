#lang racket

(define value-of
  (lambda (expr env)
    (match expr
      [`(const ,expr) expr]
      [`(mult ,x1 ,x2) (* (value-of x1 env) (value-of x2 env))]
      [`(sub1 ,x) (sub1 (value-of x env))]
      [`(zero ,x) (zero? (value-of x env))]
      [`(if ,test ,conseq ,alt) (if (value-of test env)
                                    (value-of conseq env)
                                    (value-of alt env))]
      [`(letcc ,body) (let/cc k
                         (value-of body (lambda (y) (if (zero? y) k (env (sub1 y))))))]
      [`(throw ,k-exp ,v-exp) ((value-of k-exp env) (value-of v-exp env))]
      [`(let ,e ,body) (let ((a (value-of e env)))
                         (value-of body (lambda (y) (if (zero? y) a (env (sub1 y))))))]
      [`(var ,expr) (env expr)]
      [`(lambda ,body) (lambda (a) (value-of body (lambda (y) (if (zero? y) a (env (sub1 y))))))]
      [`(app ,rator ,rand) ((value-of rator env) (value-of rand env))])))

; Expr ::=
;        | (const Integer)
;        | (mult Expr Expr)
;        | (sub1 Expr)
;        | (zero Expr)
;        | (if   Expr Expr Expr)
;        | (let/cc Expr)
;        | (throw Expr Expr)
;        | (var Var)
;        | (lambda Expr)
;        | (app Expr Expr)

; Value ::= Integer
;         | Boolean
;         | Closure

; Cont = Value → Value

; Env = Var × Cont → Value

; Closure = Value × Cont → Value

; value-of-cps : Expr × Env × Cont → Value
(define value-of-cps
  (λ (expr env return)
    (match expr
      [`(const ,n/b) (apply-k return n/b)]
      [`(var ,x) (lookup env x return)]
      [`(mult ,e0 ,e1) (value-of-cps e0 env (λ (n0)
		       (value-of-cps e1 env (λ (n1^)
		       (apply-k return (* n0 n1^))))))]
      [`(sub1 ,e) (value-of-cps e env (λ (n)
		  (apply-k return (sub1 n))))]
      [`(zero ,e) (value-of-cps e env (λ (n) 
		  (apply-k return (zero? n))))]
      [`(if ,e0 ,e1 ,e2) (value-of-cps e0 env (λ (b)
			 (if b 
			     (value-of-cps e1 env return)
			     (value-of-cps e2 env return))))]
      [`(letcc ,e) (value-of-cps e (extend-env return env) return)]
      [`(throw ,e0 ,e1) (value-of-cps e0 env (λ (return)
			(value-of-cps e1 env (λ (v^)
			(apply-k return v^)))))]
      [`(let ,e0 ,e1) (value-of-cps e0 env (λ (v)
		      (value-of-cps e1 (extend-env v env) return)))]
      [`(lambda ,e) (apply-k return (closure e env))]
      [`(app ,e0 ,e1) (value-of-cps e0 env (λ (clos)
		      (value-of-cps e1 env (λ (v^)
		      (apply-closure clos v^ return)))))])))

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