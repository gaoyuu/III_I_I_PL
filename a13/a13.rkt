#lang racket
;yu gao

(require "mk.rkt")
(require "numbers.rkt")

;operate similarly to Racket's list?
(define listo
  (λ (ls)
    (fresh (q)
           (conde
            ((== ls '()) (== q #t))
            ((fresh (f r)
                    (== `(,f . ,r) ls)
                    (listo r)))))))


;facto
(define facto
  (λ (q result)
    (conde
     ((== q '()) (== result '(1)))
     ((fresh (temp n)
             (facto temp n)
             (*o q n result)
             (minuso q '(1) temp))))))
;(run 1 (q) (facto  q '(0 0 0 1 1 1 1)))
;((0 0 0 1 1 1 1))

(define fibs
  (lambda (n)
    (cond
      ((eqv? n 0) (values 1 1))
      (else
       (let ((n- (- n 1)))
         (let-values (((u v) (fibs n-)))
           (let ((u+v (+ u v)))
             (values v u+v))))))))

;fibso
(define fibso
  (λ (n x y)
    (conde
     [(zeroo n) (== x '(1)) (== y '(1))]
     [(fresh (sub1-n sub1-x sub1-y z)
             (minuso n '(1) sub1-n)
             (fibso sub1-n sub1-x sub1-y)
             (pluso sub1-x sub1-y z)
             (== sub1-y x)
             (== z y))])))


;ass3 fo-eulav
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

;fo-lavo(most parts referred to notes discussed during lecture)
(define fo-lavo
  (λ (aa vars vals z)
    (conde
     ((symbolo aa) (lookupo vars vals aa z))
     ((== aa `(,z etouq))
      (absento 'closure z)
      (absento 'etouq vars))
     ((fresh (bb)
             (appendo bb '(tsil) aa)
             (absento 'list vars)
             (fo-lav bb vars vals z)))
     ((fresh (x b)
             (== aa `(,b (,x) adbmal))
             (absento 'adbmal vars)
             (symbolo x)
             (== z `(closure ,x ,b ,vars ,vals))))
     ((fresh (rator rand)
             (== aa `(,rand ,rator))
             (fresh (x b vars^ vals^ a)
                    (fo-lavo rator vars vals `(closure ,x ,b ,vars^ ,vals^))
                    (fo-lavo rand vars vals a)
                    (fo-lavo b `(,x . ,vars^) `(,a . ,vals^) z)))))))

(define lookupo
  (λ (vars vals y z)
    (fresh (var val var-rest val-rest)
           (== `(,var . ,var-rest) vars)
           (== `(,val . ,val-rest) vals)
           (conde
            ((== var y) (== val z))
            ((lookupo var-rest val-rest y z))))))

(define val-ofo*
  (λ (cc vars vals z)
    (conde
     ((== `() cc) (== z `()))
     ((fresh (exp exps)
             (== cc `(,exp . ,exps))
             (fresh (v v^)
                    (== z `(,v . ,v^))
                    (val-ofo* exps vars vals v^)
                    (val-ofo exp vars vals v)))))))
;fo-lav
(define fo-lav
  (λ (dd vars vals z)
    (conde
     ((== '() dd)
      (== '() z))
     ((fresh (exp exps)
             (== `(,exp . ,exps) dd)
             (fresh (v vs)
                    (== z `(,v . ,vs))
                    (fo-lav exps vars vals vs)
                    (fo-lavo exp vars vals v)))))))
;val-ofo
(define val-ofo
  (λ (ee vars vals z)
    (conde
     ((symbolo ee) (lookupo vars vals ee z))
     ((== ee `(quote ,z))
      (absento 'closure z)
      (absento 'quote vars))
     ((fresh (exps)
             (== ee `(list . ,exps))
             (absento 'list vars)
             (val-ofo* exps vars vals z)))
     ((fresh (x b)
             (== ee `(λ (,x) ,b))
             (absento 'λ vars)
             (symbolo x)
             (== z `(closure ,x ,b ,vars ,vals))))
     ((fresh (rator rand)
             (== ee `(,rator ,rand))
             (fresh (x b vars^ vals^ a)
                    (val-ofo rator vars vals `(closure ,x ,b ,vars^ ,vals^))
                    (val-ofo rand vars vals a)
                    (val-ofo b `(,x . ,vars^) `(,a . ,vals^) z)))))))

;;color-middle-earth
(define middle-earth
  '((lindon eriador forodwaith)
    (forodwaith lindon rhovanion eriador)
    (eriador lindon forodwaith rhovanion enedwaith)
    (rhovanion forodwaith eriador enedwaith rohan rhun)
    (enedwaith eriador rhovanion rohan gondor)
    (rohan enedwaith rhovanion rhun gondor mordor)
    (gondor enedwaith rohan mordor)
    (rhun rohan rhovanion khand mordor)
    (mordor gondor rohan rhun khand harad)
    (khand mordor rhun harad)
    (harad mordor khand)))

(define membero
  (lambda (x ls)
    (fresh (a b z)
           (== ls `(,a . ,b))
           (conde
            [(== x a) (== z x)]
            [(=/= x a) (membero x b)]))))

(define color-middle-earth
  '(remain undone))

(define lookupo1
  (lambda (env x b)
    (fresh (aa da d)
           (== env ` ((,aa . ,da) . ,d))
           (conde
            ((== aa x) (== da b))
            ((=/= aa x) (lookupo1 d x b))))))
