#lang racket
;yu gao

;1.filter
(define filter
  (lambda (proc ls)
    (cond
      [(null? ls) '()]
      [(equal? (proc (car ls) ) #t) (cons (car ls) (filter proc (cdr ls)))]
      [else (filter proc (cdr ls))])))

(define (filter-sps proc ls ans)
  (cond
    [(null? ls) (values '() ans)]
    [else
     (let-values ([(x y) (filter-sps proc (cdr ls) ans)])
       (if (proc (car ls))
           (values (cons (car ls) x) y)
           (values x (cons (car ls) y))))]))

(filter-sps even? '(1 2 3 4 5 6 7 8 9 10) '())

;2.filter*
(define filter*
  (lambda (f ls)
    (cond
      [(null? ls) '()]
      [(null? (car ls)) '()]
      [(pair? (car ls))
       (cons (filter* f (car ls)) (filter* f (cdr ls)))]
      ;[(null? (car ls)) '()]
      [(f (car ls)) (cons (car ls) (filter* f (cdr ls)))]
      [else (filter* f (cdr ls))])))

(define (filter*-sps f ls ans)
  (cond
    [(null? ls) (values '() ans)]
    [(null? (car ls)) (values '() ans)]
    [(pair? (car ls)) (let-values ([(x y) (filter*-sps f (car ls) ans)])
                        (let-values ([(x1 y1) (filter*-sps f (cdr ls) ans)])
                          (values (cons x x1) (cons y y1))))]
    [else (let-values ([(x2 y2) (filter*-sps f (cdr ls) ans)])
            (if (f (car ls))
                (values (cons (car ls) x2) y2)
                (values x2 (cons (car ls) y2))))]))

(filter* (lambda (x) (or (even? x) (< 7 x))) '(1 (2 3 (4 5)) 6 7 ((8 9) 10)))
;3.fibs most parts have been discussed during lecture
(define (fib-sps n ans)
  (cond
    ;[(zero? n) (values n '((0 . 0)))]
    ;[(eqv? n 1) (values n '((1 . 1)))]
    ;class notes below
    [(assv n ans) => (λ (pr)
                       (values (cdr pr) ans))]
    [(< n 2) (values n (cons `(,n . ,n) ans))]
    [else
     (let-values ([(x y) (fib-sps (- n 2) ans)])
       (let-values ([(x1 y1) (fib-sps (- n 1) y)])
         (values (+ x x1) (cons (cons n (+ x x1)) y1))))]))

(fib-sps 10 '())

;macro (*and)
(define-syntax and*
  (syntax-rules ()
    [(_) #t]
    [(_ #f) #f]
    [(_ ,x) ,x]
    [(_ a : b) b]))

(and* 1 2 3)
(and* #f)
(and*)
(and* 'a)

;list*
(define-syntax list*
  (syntax-rules ()
    [(_ `,x) ,x]
    [(_ 'a ... b) (append `(a ...)  b)]
    [(_) (raise-syntax-error "error~: no input")]))

(list* 'a 'b 'c 'd)
(list* 'a)

;macro-list
(define-syntax macro-list
  (syntax-rules ()
    [(_) '()]
    [(_ a ... b) (append '(a ... b) '())]))

(macro-list 1 'b 2 'd)
(macro-list)

;mcond
(define-syntax mcond
  (syntax-rules ()
    [(_ (#f whatever)
        (else x)) x]
    [(_ (else x)) x]
    [(_ (#t x) 
         (unbound variables)) x]))

(mcond
    (#f #t)
    (else 'dog))
(mcond 
    (else 'cat))
(mcond 
    (#t #t) 
    (unbound variables))

;macro-map
(define-syntax macro-map
  (syntax-rules ()
    [(_ proc  '((a ... b)
                (c ... d)
                (e ... f)))
     `(,(proc (a ... b))
       ,(proc (c ... d))
       ,(proc (e ... f)))]))

(define-syntax copy-code
    (syntax-rules ()
      [(_ x) `(,x x)]))

(define-syntax quote-quote
    (syntax-rules ()
      [(_ e) (quote (quote e))]))

(map (lambda (x) (list x x)) '(a b c))
(copy-code (lambda (x) x))
(copy-code 'a)
;(map copy-code '(a b c)) bad syntax
(macro-map quote '((trinidad and tobago) (saint vincent and the grenadines) (antigua and barbuda)))
(macro-map copy-code '((lambda (x) x) (lambda (x) (+ 2 x)) (lambda (x) 7)))
(macro-map quote-quote '((trinidad and tobago) (saint vincent and the grenadines) (antigua and barbuda)))