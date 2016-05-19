#lang racket
;;;yu gao
(define value-of
  (lambda (exp env)
    (match exp
      [`,y #:when (symbol? y) (env exp)]
      [`,y #:when (boolean? y) y]
      [`(lambda (,x) ,body) (lambda (v) (value-of body (lambda (y) (if (eqv? x y)
                                                                       v
                                                                       (env y)))))]
      [`(let ([,x ,y]) ,body) (value-of body (extend-env-fn x (value-of y env) env))]
      [(? number?) exp]
      [`(zero? ,e) (zero? (value-of e env))]
      [`(sub1 ,e) (sub1 (value-of e env))]
      [`(* ,e1 ,e2) (* (value-of e1 env) (value-of e2 env))]
      [`(if ,e1 ,e2 ,e3) (if (value-of e1 env)
                             (value-of e2 env)
                             (value-of e3 env))]
      [`(,rator ,rand) ((value-of rator env) (value-of rand env))])))


(define value-of-fn
  (lambda (exp env)
    (match exp
      [`,y #:when (symbol? y) (env exp)]
      [`,y #:when (boolean? y) y]
      [`(lambda (,x) ,body) (lambda (v) (value-of-fn body (lambda (y) (if (eqv? x y)
                                                                       v
                                                                       (env y)))))]
      [`(let ([,x ,y]) ,body) (value-of-fn body (extend-env-fn x (value-of-fn y env) env))]
      [(? number?) exp]
      [`(zero? ,e) (zero? (value-of-fn e env))]
      [`(sub1 ,e) (sub1 (value-of-fn e env))]
      [`(* ,e1 ,e2) (* (value-of-fn e1 env) (value-of e2 env))]
      [`(if ,e1 ,e2 ,e3) (if (value-of-fn e1 env)
                             (value-of-fn e2 env)
                             (value-of-fn e3 env))]
      [`(,rator ,rand) ((value-of-fn rator env) (value-of-fn rand env))])))

(define empty-env-fn
  (lambda ()
    (lambda (x) (write "Invalid sym"))))

(define extend-env-fn
  (lambda (x a env)
    (lambda (y)
      (if (eqv? x y)
          a
          (apply-env-fn env y)))))

(define apply-env-fn
  (lambda (env x)
    (env x)))

(define value-of-ds
  (lambda (exp env)
    (match exp
      [`,y #:when (symbol? y) (apply-env-ds env exp)]
      [`,y #:when (boolean? y) y]      
      [`,n #:when (number? n) n]      
      [`(if ,a ,b ,c) (if (value-of-ds a env) (value-of-ds b env) (value-of-ds c env))]
      [`(zero? ,a) (zero? (value-of-ds a env))]
      [`(sub1 ,x) (sub1 (value-of-ds x env))]
      [`(lambda (,x) ,body) (lambda (a) (value-of-ds body (extend-env-ds x a env)))]
      [`(let ([,x ,y]) ,body) (value-of-ds body (extend-env-ds x (value-of-ds y env) env))]
      [`(* ,x ,y) (* (value-of-ds x env) (value-of-ds y env))]
      [`(,rator ,rand) ((value-of-ds rator env) (value-of-ds rand env))])))

(define empty-env-ds
  (lambda ()
    (list 'empty-env)))

(define extend-env-ds
  (lambda (x a env)
    (list 'extend-env x a env)))

(define apply-env-ds 
   (lambda (env x) 
     (if (eqv? (car env) 'empty-env) 
         (write "Invalid") 
       (if(eqv? (car env) 'extend-env) 

          (if (eqv? x (cadr env)) 
              (caddr env)
              (apply-env-ds (cadddr env) x)) 
          (write "Invalid env")))))
  
      


(define fo-eulav
  (lambda (exp env)
    (match exp
      [`,n #:when (number? n) n]
      [`,x #:when (symbol? x) (env x)]
      [`,b #:when (boolean? b) b]
      [`(,e3 ,e2 ,e1, fi) (if (fo-eulav e1 env)
                              (fo-eulav e2 env)
                              (fo-eulav e3 env))]
      [`(,x ?orez) (zero? (fo-eulav x env))]
      [`(,x 1bus) (sub1 (fo-eulav x env))]
      [`(,body (,x) adbmal) (lambda (a) (fo-eulav body (lambda (y) (if (eqv? y x) a (env y)))))]

      [`(,x ,y *) (* (fo-eulav x env) (fo-eulav y env))]
      [`(,rand ,rator) ((fo-eulav rator env) (fo-eulav rand env))])))

(define empty-env
  (lambda ()
    (empty-env-fn)))