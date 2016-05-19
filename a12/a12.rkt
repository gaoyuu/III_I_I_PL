;;yu gao
#lang racket
(require "monads.rkt")

;;1.maybe monad:The function assv takes an association list and a value to look up.
(define assv-maybe
  (λ (n ls)
    (cond
      [(null? ls) (fail)];;nothing defined in fail in monads.rkt
      [(eqv? (caar ls) n) (return-maybe (cdar ls))]
      [else
       (assv-maybe n (cdr ls))])))

;;2. The function partition takes a list and a predicate, returning a dotted pair with the values
;;that do not pass the predicate in the first position and the values that do in the second position.

;(define return-writer
;  (lambda (a) `(,a . ())))
;(define bind-writer
;  (lambda (ma f)
;    (match-let* ((`(,a . ,la) ma)
;                 (`(,b . ,lb) (f a)))
;      `(,b . ,(append la lb)))))
;(define tell-writer
;  (lambda (msg)
;    `(_ . (,msg))))
(define partition-writer
  (λ (pred ls)
    (cond
      [(null? ls) (return-writer '())]
      [(pred (car ls))
       (bind-writer (tell-writer (car ls))
                    (λ (_)   
                      (partition-writer pred (cdr ls))))]
      [else
       (bind-writer (return-writer (car ls))
                    (λ (a)   
                      (let ([y (partition-writer pred (cdr ls))])
                        `(,(cons a (car y)) . ,(cdr y)))))])))
(partition-writer even? '(1 2 3 4 5 6 7 8 9 10))
;;3. power
(define power
  (lambda (x n)
    (cond
      [(zero? n) 1]
      [(= n 1) x]
      [(odd? n) (* x (power x (sub1 n)))]
      [(even? n) (let ((nhalf (/ n 2)))
                   (let ((y (power x nhalf)))
                     (* y y)))])))
;;(powerXpartials 3 5)
(define powerXpartials
  (λ (x n)
    (cond
      [(zero? n) (return-writer 0)]
      [(= n 1) (return-writer x)]
      [(odd? n) 
       (bind-writer (powerXpartials x (sub1 n))
                    (λ (x^) (bind-writer (tell-writer x^)
                                         (λ (_) (return-writer (* x x^))))))]
      [(even? n)
       (bind-writer (powerXpartials x (/ n 2))
                    (λ (x^) (bind-writer (tell-writer x^)
                                         (λ (_) (return-writer (* x^ x^))))))])))
;;4.Given a symbol x and a deeply-nested list of symbols ls*
;;via a preorder walk replace every occurrence of x with the number of xs that have been seen so far.
;;((replace-with-count 'o '(a o (t o (e o t ((n) o))))) 0)
;(define return-state
;  (lambda (a)
;    (lambda (s)
;      `(,a . ,s))))
;(define bind-state
;  (lambda (ma f)
;    (lambda (s)
;      (match-let ((`(,v . ,s^) (ma s)))
;        ((f v) s^)))))
;(define get-state
;  (lambda ()
;   (lambda (s) `(,s . ,s))))
;(define put-state
;  (lambda (new-s)
;    (lambda (s)
;      `(_ . ,new-s))))
;(define (replace-with-count sym ls))
;remain undone

(define traverse
    (lambda (return bind f)
      (letrec
        ((trav
           (lambda (tree)
             (cond
               [(pair? tree)
                (do bind
                  (a <- (trav (car tree)))
                  (d <- (trav (cdr tree)))
                  (return (cons a d)))]
               [else (f tree)]))))
        trav)))
;;5. The reciprocal of a number n is computed by (/ 1 n).
(define reciprocal
  (λ (n)
    (cond
      [(zero? n) (fail)]
      [else
       (return-maybe (/ 1 n))])))

(define traverse-reciprocal
  (traverse return-maybe bind-maybe reciprocal))
;;6. Halve. Implement the function halve that,
;;given a number, either will return in the monad half the number
(define halve
  (λ (n)
    (cond
      [(even? n) (return-writer (/ n 2))]
      [else (bind-writer (tell-writer n)
                         (λ (return)
                           (return-writer n)))])))

(define traverse-halve
    (traverse return-writer bind-writer halve))
;;7.State/sum. Implement a function state/sum which will, when given a number,
;;return the current state as the value, and add that number to the current state.
(define state/sum
  (λ (n)
    (λ (s)
      ((return-state s) (+ n s)))))

(define traverse-state/sum
    (traverse return-state bind-state state/sum))

(define bbtd×reverse
  (lambda (ls)
    (match ls
      (`() ( return-writer 0))
      (`(,a . ,d)
        (do bind-writer
          (rec <- (bbtd×reverse d))
          ( tell-writer a)
          ( return-writer (+ a (* 2 rec))))))))