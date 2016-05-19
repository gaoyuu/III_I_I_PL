#lang racket
;yu gao
(require "mk.rkt")
(require "numbers.rkt")

;; Part I Write the answers to the following problems using your
;; knowledge of miniKanren.  For each problem, explain how miniKanren
;; arrived at the answer.  You will be graded on the quality of your
;; explanation; a full explanation will require several sentences.

;; 1 What is the value of 

(run 2 (q)
  (== 5 q)
  (conde
   [(conde 
     [(== 5 q)
      (== 6 q)])
    (== 5 q)]
   [(== q 5)]))
;return '(5)
;conde: every successful conde line contributes one or more values.
;in the second line, q is already bound by 5, so for the first case in inner conde,
;(== 6 q) does not succeed, so it does not pass. for the bottom two lines. (== 5 q)
;and (==q 5) are the same. they are bound with the same value which is 5 as the second
;line says. But there is no fresh calls to return value, so the final result only returns
;a list with one 5.


;; 2 What is the value of
(run 1 (q) 
  (fresh (a b) 
    (== `(,a ,b) q)
    (absento 'tag q)
    (symbolo a)))
;return '(((_.0 _.1) (=/= ((_.0 tag))) (sym _.0) (absento (tag _.1))))
;1st, for (_.0 _.1), it means q represents (a b) which a and b are freshed but not bounded.
;a is the 0th value, b is the 1st value.
;2nd, If a is asserted to be a symbol, the absento constraint on a can be simplified to a disequality
;constraint between a and tag. which is the second ouput.
;3rd, a is the 0th value which is a symbol 
;4th, The answer states that the two unbound variables,
;    a and b, cannot be associated with a term that contains the term tag
;
;; 3 What do the following miniKanren constraints mean?
;http://io.livecode.ch/learn/webyrd/webmk
;; a ==       ;;equal equal, judge whether two arguments are equal
;; b =/=      ;;not equal
;; c absento  ;;does not contain
              ;;as the example states in the url
(run* (q)
  (fresh (x y)
    (== `(jackal (,y leopard ,x)) q)
    (absento 'panda q)))
              ;;panda can not be in the group which has jackal and leopard
;; d numbero  ;;quite similar with number? in scheme
              ;;try (run 1 (q) (fresh (a) (numbero a))) returns '(_.0)
              ;;try (run* (q) (symbolo q)) returns ((_.0 (sym _.0)))
              ;;q is a symbol but unbounded
;; e symbolo  ;;cquite similar with symbol? in scheme
              ;;try (run 1 (q) (fresh (a) (symbolo a))) returns '(_.0)
              ;;try (run* (q) (numbero q)) returns ((_.0 (num _.0)))
              ;;q is a number but unbounded

;; Part II goes here.
;assoc
;(define assoc
;  (lambda (x ls)
(define (assoco x ls q)
;    (match-let* ((`(,a . ,d) ls)
;                 (`(,aa . ,da) a))
      (fresh (a d)
           (== `(,a . ,d) ls)
           (fresh (aa da)
                  (== `(,aa . ,da) a)
;                 (cond
;                 ((equal? aa x) a)
;                 ((not (equal? aa x)) (assoc x d))))))
                  (conde 
                  ((== aa x) (== q a))
                  ((=/= x aa) (assoco x d q))))))


;reverse
;(define reverse
;  (lambda (ls)
;    (cond
(define (reverseo ls q)
  (conde
;      ((equal? '() ls) '())
      ((== '() ls) (== '() q))
;      (else
      ((=/= '() ls) (fresh (a d)
                        (== `(,a . ,d) ls)
;       (match-let* ((`(,a . ,d) ls)
;                   (res (reverse d)))
;       (append res `(,a)))))))
                        (fresh (res)
                               (reverseo d res)
                               (appendo res `(,a) q))))))

;stutter
;(define stutter
;  (lambda (ls)
;    (cond
(define (stuttero ls q)
  (conde
;      ((equal? '() ls) '())
;      (else
      ((== '() ls) (== '() q))
      ((=/= '() ls) (fresh (a d)
                           (== `(,a . ,d) ls)
;        (match-let* ((`(,a . ,d) ls)
;		     (res (stutter d)))
;          `(,a ,a . ,res))))))
                           (fresh (res)
                               (== q `(,a ,a . ,res))
                               (stuttero d res))))))