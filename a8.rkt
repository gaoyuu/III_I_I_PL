#lang racket
;;Yu Gao

(define k* #f)
(define m* #f)
(define n* #f)
(define v* #f)
(define ls* '())
(define empty-k-reg
  (lambda ()
    `(empty-k-reg)))
;;ack
(define ack
  (lambda (m n k)
    (cond
      [(zero? m) (k (add1 n))]
      [(zero? n) (ack (sub1 m) 1 k)]
      [else (ack m (sub1 n) (lambda (v) (ack (sub1 m) v k)))])))

(define ack-k-reg
  (lambda (m k)
    `(ack-k-reg ,m ,k)))

(define apply-k-ack
  (lambda ()
    (match k*
      [`(empty-k-reg) v*]
      [`(ack-k-reg ,m ,k^)
       (begin (set! m* (sub1 m))
              (set! n* v*)
              (set! k* k^)
              (ack-reg))])))
(define ack-reg
  (lambda ()
    (cond
      [(zero? m*) (begin
                    (set! k* k*)
                    (set! v* (add1 n*))
                    (apply-k-ack))]
      [(zero? n*) (begin
                    (set! k* k*)
                    (set! m* (sub1 m*))
                    (set! n* 1)(ack-reg))]
      [else (begin
              (set! k* (ack-k-reg m* k*))
              (set! m* m*)
              (set! n* (sub1 n*))
              (ack-reg))])))
(define ack-reg-driver
  (lambda (m n)
    (set! k* (empty-k-reg))
    (set! m* m)
    (set! n* n)
    (set! v* 'empty)
    (ack-reg)))

;;depth
(define depth
  (lambda (ls k)
    (cond
      [(null? ls) (k 1)]
      [(pair? (car ls))
       (depth (car ls)
	      (lambda (l)
		(depth (cdr ls)
		       (lambda (r)
			 (let ((l (add1 l)))
			   (if (< l r) (k r) (k l)))))))]
      [else (depth (cdr ls) k)])))

(define depth-inner-k-reg
  (lambda (v^ k)
    `(depth-inner-k-reg ,v^ ,k)))

(define depth-outer-k-reg
  (lambda (ls k)
    `(depth-outer-k-reg ,ls ,k)))

(define depth-reg
  (lambda ()
    (cond
      [(null? ls*)
       (begin
         (set! k* k*)
         (set! v* 1)
         (apply-k-depth))]
      [(pair? (car ls*))
       (begin
         (set! k* (depth-outer-k-reg ls* k*))
         (set! ls* (car ls*))
         (depth-reg))]
      [else (begin
              (set! ls* (cdr ls*))
              (set! k* k*)
              (depth-reg))])))


(define apply-k-depth
  (lambda ()
    (match k*
      [`(empty-k-reg) v*]
      [`(depth-inner-k-reg ,v^ ,k^)
       (let ((v^ (add1 v^)))
         (begin
           (set! k* k^)
           (if (< v^ v*)(set! v* v*)(set! v* v^))
           (apply-k-depth)))]             
      [`(depth-outer-k-reg ,ls ,k^)
       (begin
         (set! ls* (cdr ls))
         (set! k* (depth-inner-k-reg v* k^))
         (depth-reg))])))
(define depth-reg-driver
  (lambda (ls)
    (set! k* (empty-k-reg))
    (set! ls* ls)
    (set! v* 'empty)
    (depth-reg)))

;;fact
 
(define fact
  (lambda (n k)
    ((lambda (fact k)
       (fact fact n k))
     (lambda (fact n k)
       (cond
         [(zero? n) (k 1)]
         [else (fact fact (sub1 n) (lambda (v) (k (* n v))))]))
     k)))
(define fact-k-reg
  (lambda (n k)
    `(fact-k-reg ,n ,k)))

(define fact-reg
  (lambda () 
    ((lambda (fact-reg)
       (begin
         (set! n* n*)
         (set! k* k*)
         (fact-reg fact-reg)))
     (lambda (fact-reg)
       (cond
         [(zero? n*) (begin
                       (set! k* k*)
                       (set! v* 1)
                       (apply-k-fact))]
         [else (begin
                 (set! k* (fact-k-reg n* k*))
                 (set! n* (sub1 n*))
                 (fact-reg fact-reg))])))))

(define apply-k-fact
  (lambda ()
    (match k*
      [`(empty-k-reg) v*]
      [`(fact-k-reg ,n ,k^) (begin
                              (set! k* k^)
                              (set! v* (* n v*))
                              (apply-k-fact))])))
      
(define fact-reg-driver
  (lambda (n)
    (set! k* (empty-k-reg))
    (set! v* 'empty)
    (set! n* n)(fact-reg)))

;;pascal
(define pascal
  (lambda (n k)
    (let ((pascal
           (lambda (pascal k)
             (k (lambda (m a k)
		  (cond
		    [(> m n) (k '())]
		    [else (let ((a (+ a m)))
			    (pascal pascal (lambda (f) (f (add1 m) a (lambda (v) (k (cons a v)))))))]))))))
      (pascal pascal (lambda (f) (f 1 0 k))))))

(define pascal-inner-k-reg
  (lambda (a k)
    `(pascal-inner-k-reg ,a ,k)))

(define pascal-outer-k-reg
  (lambda (m a k)
    `(pascal-outer-k-reg ,m ,a ,k)))

;;;;