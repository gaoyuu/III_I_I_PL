
;Yu Gao
#lang racket

(define lex
  (lambda (expr ls)
    (match expr
      (`,y #:when (symbol? y) `(var ,
                         (if (boolean? (assv expr ls)) 
                             0 
                             (cadr (assv expr ls)))))
      (`,n #:when (number? n) `(const ,n))
;;boolean depends in specific condition or it will fail when referring env   (`,b #:when (boolean? b) 0)
      (`(lambda (,x) ,body) `(lambda ,(lex body (if (boolean? (assv x ls))
                                                   (cons (cons x (cons 0 '())) (map (lambda (lis) (list (car lis) (add1 (cadr lis)))) ls))
                                                    (cons (cons x (cons 0 '())) ((remove (assv x ls) ls)))))))
      (`(,rator ,rand) (list (lex rator ls) (lex rand ls)))
      (`(if ,e1 ,e2 ,e3) `(if ,(lex e1 ls)
                             ,(lex e2 ls)
                             ,(lex e3 ls)))
      (`(zero? ,e) `(zero? ,(lex e ls)))
      (`(sub1 ,e) `(sub1 ,(lex e ls)))
      (`(* ,e1 ,e2) `(* ,(lex e1 ls) ,(lex e2 ls)))
      (`(let [(,x ,val)] ,body) 
       `(let ,(lex val ls) ,(lex body ls)))
      (`(,rator ,rand) `(,(lex rator ls) ,(lex rand ls))))))


;;''value-of-fn'' should use a functional representation of closures.
(define empty-env
  (lambda ()
    '()))
(define extend-env
  (lambda (x a env)
    `((,x . ,a) . ,env)))
(define apply-env
  (lambda (env y)
    (cond
      [(assq y env)   (cdr (assq y env) )]
      [else (error 'env "Unbounded variables ~s" y)])))
(define closure-fn
  (lambda (x body env)
    (lambda (arg)
      (value-of-fn body (extend-env x arg env)))))
(define apply-closure-fn
  (lambda (a arg)
    (a arg)))
(define value-of-fn
  (lambda (exp env)
    (match exp
      [`,x #:when (symbol? x) (apply-env env exp)]
      [`,n #:when (number? n) n]
      [`,b #:when (boolean? b) b]
      [`(lambda (,x) ,body) (closure-fn x body env)]
      [`(zero? ,e) (zero? (value-of-fn e env))]
      [`(* ,e1 ,e2) (* (value-of-fn e1 env) (value-of-fn e2 env))]
      [`(sub1 ,e) (sub1 (value-of-fn e env))]
      [`(let ([,x ,y]) ,body) (value-of-fn body (extend-env x (value-of-fn y env) env))]
      [`(if ,e1 ,e2 ,e3) (if (value-of-fn e1 env)
                             (value-of-fn e2 env)
                             (value-of-fn e3 env))]
      [`(,rator ,rand) (apply-closure-fn (value-of-fn rator env) (value-of-fn rand env))])))

;;''value-of-ds'' should use a data-structural representation of closures.
(define closure-ds
  (lambda (x body env)
    `(closure ,x ,body ,env)))
(define apply-closure-ds
  (lambda (closure a)
    (match closure
      [`(closure ,x ,body ,env) (value-of-ds body (extend-env x a env))])))
(define value-of-ds
  (lambda (exp env)
    (match exp
      [`,x #:when (symbol? x) (apply-env env exp)]
      [`,n #:when (number? n) n]
      [`(lambda (,x) ,body) (closure-ds x body env)]
      [`(zero? ,e) (zero? (value-of-ds e env))]
      [`(* ,e1 ,e2) (* (value-of-ds e1 env) (value-of-ds e2 env))]
      [`(sub1 ,e) (sub1 (value-of-ds e env))]
      [`(let ([,x ,y]) ,body) (value-of-ds body (extend-env x (value-of-ds y env) env))]
      [`(if ,e1 ,e2 ,e3) (if (value-of-ds e1 env)
                             (value-of-ds e2 env)
                             (value-of-ds e3 env))]
      [`(,rator ,rand) (apply-closure-ds (value-of-ds rator env) (value-of-ds rand env))])))

;;3
(define closure-scopes
  (case-lambda 
    [(x body env) `(closure ,x ,body ,env)]
    [(x body) `(closure ,x ,body)]))

(define apply-closure-scopes
  (lambda (closure a env)
     (match closure
       [`(closure ,x ,body ,env) (value-of-scopes body (extend-env x a env))]
       [`(closure ,x ,body) (value-of-scopes body (extend-env x a env))])))

(define value-of-scopes
  (lambda (exp env)
    (match exp
      [`,x #:when (symbol? x) (apply-env env exp)]
      [`,n #:when (number? n) n]
      [`,b #:when (boolean? b) b]
      [`(null? ,e) (equal? (value-of-scopes e env) '())]
      [`(cons ,e1 ,e2) (cons (value-of-scopes e1 env) (value-of-scopes e2 env))]
      [`(car ,e) (car (value-of-scopes e env))]
      [`(cdr ,e) (cdr (value-of-scopes e env))]
      [`(let ((,x ,y)) ,body) (let ((a (value-of-scopes y env))) (value-of-scopes body (extend-env x a env)))]
      [`(lambda (,x) ,body) (closure-scopes x body env)]
      [`(d-lambda (,x) ,body) (closure-scopes x body)]
      [`(quote()) '()]
      [`(if ,e1 ,e2 ,e3) (if (value-of-scopes e1 env)
                             (value-of-scopes e2 env)
                             (value-of-scopes e3 env))]
      [`(,rator ,rand) (apply-closure-scopes (value-of-scopes rator env) 
                                             (value-of-scopes rand env)
                                             env)])))

;;value-of-dynamics
(define value-of-dynamic
  (lambda (expr env)
    (match expr
      [`,x #:when (symbol? x) (apply-env env x)]
      [`,x #:when (number? x) x]
      [`,b #:when (boolean? b) b]
      [`(lambda (,x) ,body) `(lambda (,x) ,body)]
      [`(let ((,a ,b)) ,body)
       (let ((c (value-of-dynamic b env)))
         (value-of-dynamic body (extend-env a c env)))]
      [`(if ,e1 ,e2 ,e3)
       (if (value-of-dynamic e1 env)
           (value-of-dynamic e2 env)
           (value-of-dynamic e3 env))]
      [`(* ,e1 ,e2) (* (value-of-dynamic e1 env) (value-of-dynamic e2 env))]
      [`(+ ,e1 ,e2) (+ (value-of-dynamic e1 env) (value-of-dynamic e2 env))]
      [`(sub1 ,num) (sub1 (value-of-dynamic num env))]
      [`(null? ,ls) (null? (value-of-dynamic ls env))]
      [`(zero? ,num) (zero? (value-of-dynamic num env))]
      [`(cons ,a ,b) (cons (value-of-dynamic a env) (value-of-dynamic b env))]
      [`(car ,a) (car (value-of-dynamic a env))]
      [`(cdr ,a) (cdr (value-of-dynamic a env))]
      [`(quote ,v) v]
      [`(,rator ,rand)
;;       (match (value-of-dynamic rator env)
;;         [`(lambda (,x) ,body)
;;          (value-of-dynamic body (extend-env x (value-of-dynamic rand env) env))])]
       (match-let ([`(lambda (,x) ,body) (value-of-dynamic rator env)])
         (value-of-dynamic body (extend-env x (value-of-dynamic rand env) env)))])))