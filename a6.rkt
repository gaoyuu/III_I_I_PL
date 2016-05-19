#lang racket

;;yu gao

(define empty-k
  (lambda ()
    (let ((once-only #f))
      (lambda (v)
        (if once-only
            (error 'empty-k "You can only invoke the empty continuation once")
            (begin (set! once-only #t) v))))))

;;(define empty-k
;;  (lambda ()
;;    (lambda (v) v)))
;;prob.1
(define binary-to-decimal-cps
  (lambda (ls k)
    (cond
      [(null? ls) (k 0)]
      [else (binary-to-decimal-cps (cdr ls) (lambda (x) (k (+ (car ls) (* 2 x)))))])))

;;prob.2
(define times-cps
  (lambda (ls k)
    (cond 
      [(null? ls) (k 1)]
      [(zero? (car ls)) 0]
      [else (times-cps (cdr ls) (lambda (x) (k (* (car ls) x))))])))
;;prob.3
(define times-cps-shortcut
  (lambda (ls k)
    (cond 
      [(null? ls) (k 1)]
      ;;    [(zero? (car ls)) 0]
      [else (times-cps-shortcut (cdr ls) (lambda (x) (k (* (car ls) x))))])))
;;prob.4
(define plus-cps
  (lambda (m k)
    (k (lambda (n k)
         (k ( + m n))))))
;;prob.5
;;(define remv-first-9*-cps
;;  (lambda (ls k)
;;    ))
;;prob.6
(define cons-cell-count
  (lambda (ls)
    (cond
      [(pair? ls)
       (add1 (+ (cons-cell-count (car ls)) (cons-cell-count (cdr ls))))]
      [else 0])))
(define cons-cell-count-cps
  (lambda (ls k)
    (cond
      [(pair? ls) (cons-cell-count-cps (car ls)
                                       (lambda (x) (cons-cell-count-cps (cdr ls) 
                                                                        (lambda (y) (k (add1 (+ x y)))))))]
      [else (k 0)])))

;;prob.7
(define find-cps
  (lambda (u s k)
    (cond
      [(assv u s) (find-cps (cdr (assv u s)) s k)]
      [else (k u)])))
;;prob.8
(define ack-cps
  (lambda (m n k)
    (cond
      [(zero? m) (k (add1 n))]
      [(zero? n) (ack-cps (sub1 m) 1 k)]
      [else (ack-cps m (sub1 n) (lambda (x) (ack-cps (sub1 m) x k)))])))
;;prob.9
(define fib-cps
  (lambda (n k)
    ((lambda (fib k)
       (fib fib n k))
     (lambda (fib n k)
       (cond
         [(zero? n) (k 0)]
         [(= 1 n ) (k 1)]
         [else (fib fib (sub1 n) (lambda (x) (fib fib (sub1 (sub1 n)) (lambda (y) (k (+ x y))))))])) k)))
;;prob.10
(define null?-cps
  (lambda (ls k)
    (k (null? ls))))
(define car-cps
  (lambda (pr k)
    (k (car pr))))
(define cdr-cps
  (lambda (pr k)
    (k (cdr pr))))
(define unfold-cps
  (lambda (p f g seed k)
    ((lambda (h k)
       (h h (lambda (x) (x seed '() k))))
     (lambda (h k)
       (k (lambda (seed ans k)
            (p seed (lambda (args) (if args
                                       (k ans)
                                       (h h (lambda (x) (g seed (lambda (y) (f seed (lambda (z) (x y (cons z ans) k)))))))))))))k)))
;;prob.11
(define find 
  (lambda (u s)
    (let ((pr (assv u s)))
      (if pr (find (cdr pr) s) u))))

(define unify
  (lambda (u v s)
    (cond
      ((eqv? u v) s)
      ((number? u) (cons (cons u v) s))
      ((number? v) (unify v u s))
      ((pair? u)
       (if (pair? v)
	   (let ((s (unify (find (car u) s) (find (car v) s) s)))
             (if s (unify (find (cdr u) s) (find (cdr v) s) s) #f))
	   #f))
      (else #f))))

(define empty-s
  (lambda () 
    '()))
(define unify-cps
  (lambda (u v s k)
    (find-cps u s (lambda (x)
                    (find-cps v s
                              (lambda (y)
                                (cond
                                  [(eqv? u v) (k s)]
                                  [(symbol? u) (k `((,u . ,v) . ,s))]
                                  [(symbol? v) (k `((,v . ,u) . ,s))]
                                  [(pair? u) (if (pair? v) 
                                                 (unify-cps (car u) (car v) s (lambda (x) 
                                                                                (if x
                                                                                    (unify-cps (cdr u) (cdr v) (lambda (y)
                                                                                                                 (k y)))
                                                                                    #f))) #f)]
                                  
                                  [else (k #f)])))))))                                                    
;;prob.12
(define M-cps
  (lambda (f k)
    (k (lambda (ls k)
         (cond
           ((null? ls) (k '()))
           (else (f (car ls) (lambda (x)
                               (M-cps f (lambda (y)
                                          (y (cdr ls)
                                             (lambda (z)
                                               (k (cons x z))))))))))))))
;; prob.13
(define add1-cps
  (lambda (n k)
    (k (add1 n))))
(define use-of-M-cps
  ((M-cps add1-cps (empty-k)) '(1 2 3 4 5) (empty-k)))
;;prob.14
(define strange-cps
  (lambda (x k)
    ((lambda (g k) (k (lambda (x k) (g g k))))
     (lambda (g k) (k (lambda (x k) (g g k))))
     k)))
;;
;;
(define almost-length-cps
  (lambda (f k)
    (k (lambda (ls k)
         (if (null? ls)
             (k 0)
             (f (cdr ls) (lambda (x)
                           (k (add1 x)))))))))