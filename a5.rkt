#lang racket
;;Yu Gao
;;discuss with Yinan Zhang
(define empty-env
  (lambda ()
    '()))
(define extend-env
  (lambda (x a env)
    `((,x . ,a) . ,env)))
(define apply-env
  (lambda (env y)
    (cond
      [(assq y env) (cdr (assq y env))]
      [else   (error 'env "Invaild ~s" y)])))
(define closure
  (lambda (x body env)
    `(closure ,x ,body ,env)))
(define apply-closure
  (lambda (closure a)
    (match closure
      [`(closure ,x ,body ,env) (value-of body (extend-env x a env))])))
(define value-of
  (lambda (exp env)
    (match exp
      [`,b #:when (boolean? b) exp]
      [`,n #:when (number? n) n]
      [`,x #:when (symbol? x) (apply-env env exp)]
      [`(zero? ,n) (zero? (value-of n env))]
      [`(sub1 ,n) (sub1 (value-of n env))]
      [`(* ,n1 ,n2) (* (value-of n1 env) (value-of n2 env))]
      [`(if ,test ,conseq ,alt) (if (value-of test env)
                                  (value-of conseq env)
                                  (value-of alt env))]
      [`(begin2 ,e1 ,e2) (begin (value-of e1 env) (value-of e2 env))]
      [`(random ,n) (random (value-of n env))]
      [`(lambda (,x) ,body) (closure x body env)]
      [`(,rator ,rand) (apply-closure (value-of rator env)
                                      (value-of rand env))])))
;;call by value
(define extend-env-cbv
  (lambda (x y env)
    `((,x . ,(box y)) . ,env)))
(define apply-env-cbv
  (lambda (env x)
    (cond
      [(assq x env) (unbox (cdr (assq x env)))]
      [else (error 'env "Invaild ~s" x)])))
(define apply-closure-cbv
  (lambda (closure a)
    (match closure
      [`(closure ,x ,body ,env) (val-of-cbv body (extend-env-cbv x a env))])))

(define val-of-cbv
  (lambda (exp env)
    (match exp
      [`,b #:when (boolean? b) b]
      [`,n #:when (number? n) n]
      [`,x #:when (symbol? x) (apply-env-cbv env exp)]
      [`(quote()) '()]
      [`(lambda (,x) ,body) (closure x body env)]
      [`(null? ,e) (null? (val-of-cbv e env))]
      [`(zero? ,e) (zero? (val-of-cbv e env))]
      [`(if ,e1 ,e2 ,e3) (if (val-of-cbv e1 env)
                             (val-of-cbv e2 env)
                             (val-of-cbv e3 env))]
      [`(begin2 ,e1 ,e2) (begin (val-of-cbv e1 env) (val-of-cbv e2 env))]
      [`(add1 ,e) (add1 (val-of-cbv e env))]
      [`(sub1 ,e) (sub1 (val-of-cbv e env))]
      [`(* ,n1 ,n2) (* (val-of-cbv n1 env) (val-of-cbv n2 env))]
      [`(set! ,e1 ,e2) (let ([x (val-of-cbv e2 env)]
                             [y (apply-env env e1)]) (set-box! y x))]
      [`(,rator ,rand) (apply-closure-cbv (val-of-cbv rator env) (val-of-cbv rand env))])))

;;call by reference
(define extend-env-cbr
  (lambda (x y env)
    (if (or (box? y) (list? y))
        `((,x . ,y) . ,env)
        `((,x . ,(box y)) . ,env))))
(define apply-env-cbr
  (lambda (env x)
    (cond
      [(assq x env) (unbox (cdr (assq x env)))]
      [else (error 'env "Invaild ~s" x)])))
(define apply-closure-cbr
  (lambda (closure a)
    (match closure
      [`(closure ,x ,body ,env) (val-of-cbr1 body (extend-env-cbr x a env))])))
(define open
  (lambda (x)
    (if (box? x)
        (unbox x)
        x)))
(define val-of-cbr1
  (lambda (exp env)
    (match exp
      [`,b #:when (boolean? b) exp]
      [`,n #:when (number? n) n]
      [`,x #:when (symbol? x) (apply-env env exp)]
      [`(lambda (,x) ,body) (closure x body env)]
      [`(sub1 ,e) (sub1 (open (val-of-cbr1 e env)))]
      [`(* ,n1 ,n2) (* (open (val-of-cbr1 n1 env)) (open (val-of-cbr1 n2 env)))]
      [`(zero? ,e) (zero? (open (val-of-cbr1 e env)))]
      [`(add1 ,e) (add1 (val-of-cbr e env))]
      [`(if ,e1 ,e2 ,e3) (if (open (val-of-cbr1 e1 env))
                             (val-of-cbr1 e2 env)
                             (val-of-cbr1 e3 env))]
      [`(begin2 ,e1 ,e2) (begin (val-of-cbr1 e1 env) (val-of-cbr1 e2 env))]
      [`(set! ,e1 ,e2) (let ([x (open (val-of-cbr1 e2 env))]
                             [y (apply-env env e1)])
                         (set-box! y x))]
      [`(,rator ,rand) (apply-closure-cbr (open (val-of-cbr1 rator env)) (val-of-cbr1 rand env))])))
(define val-of-cbr
  (lambda (exp env)
    (open (val-of-cbr1 exp env))))

;;call by name

(define closure-cbname
  (lambda (x body env)
    (lambda (a)
      (val-of-cbname body (extend-env x a env)))))
(define apply-closure-cbname
  (lambda (closure a)
    (match closure 
      [`(closure ,x ,body ,env) (val-of-cbname body (extend-env x a env))])))
(define random-sieve
    '((lambda (n)
        (if (zero? n)
            (if (zero? n) (if (zero? n) (if (zero? n) (if (zero? n) (if (zero? n) (if (zero? n) #t #f) #f) #f) #f) #f) #f)
            (if (zero? n) #f (if (zero? n) #f (if (zero? n) #f (if (zero? n) #f (if (zero? n) #f (if (zero? n) #f #t))))))))
      (random 2)))

(define val-of-cbname
  (lambda (exp env)
    (match exp
      [`,x #:when (symbol? x) ((unbox (apply-env env exp)))]
      [`,b #:when (boolean? b) exp]
      [`,n #:when (number? n) n]
      [`(null? ,e) (null? (val-of-cbname e env))]
      [`(sub1 ,n) (sub1 (val-of-cbname n env))]
      [`(lambda (,x) ,body) (closure-cbname x body env)]
      [`(zero? ,n) (zero? (val-of-cbname n env))]
      [`(* ,n1 ,n2) (* (val-of-cbname n1 env) (val-of-cbname n2 env))]
      [`(if ,test ,conseq ,alt) (if (val-of-cbname test env) (val-of-cbname conseq env) (val-of-cbname alt env))]
      [`(random ,n) (random (val-of-cbname n env))]
      [`(,rator ,x) #:when (symbol? x) ((val-of-cbname rator env) (apply-env env x))]
      [`(,rator ,rand) ((val-of-cbname rator env) (box (lambda () (val-of-cbname rand env))))])))
;;call by need
(define apply-env-cbneed
  (lambda (env x)
    (env x)))
(define extend-env-cbneed
  (lambda (x val env)
    (lambda (y) 
      (if (eqv? x y)
          val 
          (apply-env-cbneed env y)))))
(define closure-cbneed
  (lambda (x body env)
    (lambda (a)
      (val-of-cbneed body (extend-env-cbneed x a env)))))

(define open1
  (lambda (t)
    (let ([loop ((unbox t))])
      (set-box! t (lambda () loop))
      loop)))
(define val-of-cbneed
  (lambda (exp env)
    (match exp
      [`,x #:when (symbol? x) (open1 (env exp))]
      [`,b #:when (boolean? b) exp]
      [`,n #:when (number? n) n]
      [`(null? ,e) (null? (val-of-cbneed e env))]
      [`(lambda (,x) ,body) (closure-cbneed x body env)]
      [`(zero? ,n) (zero? (val-of-cbneed n env))]
      [`(* ,n1 ,n2) (* (val-of-cbneed n1 env) (val-of-cbneed n2 env))]
      [`(if ,e1 ,e2 ,e3) (if (val-of-cbneed e1 env)
                             (val-of-cbneed e2 env)
                             (val-of-cbneed e3 env))]
      [`(random ,n) (random (val-of-cbneed n env))]
      [`(,rator ,x) #:when (symbol? x) ((val-of-cbneed rator env) (apply-env-cbneed env x))]
      [`(,rator ,rand) ((val-of-cbneed rator env) (box (lambda () (val-of-cbneed rand env))))])))

(define f
  (lambda (x)
    (+ 10 10)))