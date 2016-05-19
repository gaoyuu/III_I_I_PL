;;yu gao
#lang racket
(require "mk.rkt")

(define apply-Go
  (lambda (G e t)
    (fresh (a G^)
           (== `(,a . ,G^) G)
           (fresh (aa da)
                  (== `(,aa . ,da) a)
                  (conde
                   ((== aa e) (== da t))
                   ((=/= aa e) (apply-Go G^ e t)))))))

(define !-
  (lambda (G e t)
    (conde
     ((numbero e) (== 'Nat t))
     ((== t 'Bool)
      (conde
       ((== #t e))
       ((== #f e))))
     ((fresh (ne1 ne2)
             (== `(+ ,ne1 ,ne2) e)
             (== 'Nat t)
             (!- G ne1 'Nat)
             (!- G ne2 'Nat)))
     ((fresh (teste anse elsee)
             (== `(if ,teste ,anse ,elsee) e)
             (!- G teste 'Bool)
             (!- G anse t)
             (!- G elsee t)))
     ((symbolo e) (apply-Go G e t))
     ((fresh (x b)
             (== `(lambda (,x) ,b) e)
             (symbolo x)
             (fresh (tx tb)          
                    (== `(,tx -> ,tb) t)
                    (!- `((,x . ,tx) . ,G) b tb))))
     ((fresh (e1 arg)
             (== `(,e1 ,arg) e)
             (fresh (targ)
                    (!- G e1 `(,targ -> ,t))
                    (!- G arg targ))))
     
     ((fresh (n)
             (== `(sub1 ,n) e);;sub1
             (== 'Nat t)
             (!- G n 'Nat)))
     ((fresh (n1 n2)
             (== `(* ,n1 ,n2) e);;*
             (== 'Nat t)
             (!- G n1 'Nat)
             (!- G n2 'Nat)))
     ((fresh (n)
             (== `(zero? ,n) e);;zero?
             (== 'Bool t)
             (!- G n 'Nat)))
     ((fresh (rand)
             (== `(fix ,rand) e)
             (!- G rand `(,t -> ,t))))
     ((fresh (n)
             (== `(not ,n) e);;bool
             (== 'Bool t)
             (!- G n 'Bool))))))

(define baro
  (lambda (d c)
    (conde
     ((== c d)
      (fresh (e)
             (conde
              ((== ` (,e) d)))
             ((== '() d))))
     (( fresh (f g h i j k l)
              (== ` (,f . ,g) d)
              (== ` (,i . ,j) h)
              (== ` (,i . ,l) c)
              (baro g h)
              (baro k j)
              (baro ` (,f . ,k) l))))))