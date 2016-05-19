#lang racket


;;1.Define and test a procedure countdown that takes a natural number
;;and returns a list of the natural numbers less than or equal to that number,
;;in descending order.
(define countdown
  (lambda (x)
    (cond 
      [(zero? x) '(0)]
      [else (cons x (countdown (sub1 x)))])))

;;2. Define and test a procedure insertR 
;;that takes two symbols and a list and returns a new list with the second symbol
;;inserted after each occurrence of the first symbol.
(define insertR
  (lambda (x y ls)
    (cond
      [(null? ls) '()]
      [(equal? x (car ls)) (cons (car ls) (cons y (insertR x y (cdr ls))))]
      [else (cons (car ls) (insertR x y (cdr ls)))])))
;;3. Define and test a procedure remv-1st that takes a a symbol and a list 
;;and returns a new list with the first occurrence of the symbol removed.
(define remv-1st
  (lambda (x ls)
    (cond 
      [(null? ls) ls]
      [(eqv? x (car ls)) (cdr ls)]
      [else (cons (car ls) (remv-1st x (cdr ls)))])))
;;4. Define and test a procedure list-index-ofv? that takes an element and 
;a list and returns the (base 0) index of that element in the list. 
;A list missing that element will be considered bad data. 
(define list-index-ofv?
  (lambda (x ls)
    (cond 
      [(null? ls) 0]
      [(eqv?(eqv? x (car ls)) #f) (add1 (list-index-ofv? x (cdr ls)))]
      [else 0])))
      
;;5. Define and test a procedure filter that takes a predicate and a list and
;;returns a new list containing the elements that satisfy the predicate.
;;A predicate is a procedure that takes a single argument and returns either #t or #f.
;;The number? predicate, for example, returns #t if its argument is a number 
;;and #f otherwise. The argument satisfies the predicate, then, if the predicate returns #t for that argument. 
(define filter
  (lambda (proc ls)
    (cond
      [(null? ls) '()]
      [(equal? (proc (car ls) ) #t) (cons (car ls) (filter proc (cdr ls)))]
      [else (filter proc (cdr ls))])))
;;6. Define and test a procedure zip that takes two lists and forms a new list,
;;each element of which is a pair formed by combining the corresponding elements
;;of the two input lists. If the two lists are of uneven length, then drop the 
;;tail of the longer one. 
(define zip
  (lambda (ls1 ls2)
    (cond
      [(null? ls1) '()]
      [(null? ls2) '()]
      [else (cons (cons (car ls1) (car ls2)) (zip (cdr ls1) (cdr ls2)))])))
;;7. Define and test a procedure map that takes a procedure p of one argument 
;;and a list ls and returns a new list containing the results of applying p to 
;;the elements of ls. Do not use Racket's built-in map in your definition. 
(define map
  (lambda (p ls)
    (cond
      [(null? ls) '()]
      [else (cons (p (car ls)) (map p (cdr ls)))])))
;;8. Define and test a procedure append that takes two lists, ls1 and ls2, 
;;and appends ls1 to ls2. 
(define append
  (lambda (ls1 ls2)
    (cond
      [(null? ls1) ls2]
      [else (cons (car ls1) (append (cdr ls1) ls2))])))

;;9. Define and test a procedure reverse that takes a list and returns the 
;;reverse of that list. 
(define reverse
  (lambda (ls)
    (cond
      [(null? ls) '()]
      [else (append (reverse (cdr ls)) (list (car ls)))])))
;;10. Define and test a procedure fact that takes a natural number and computes
;;the factorial of that number. The factorial of a number is computed by 
;;multiplying it by the factorial of its predecessor. The factorial of 0 is defined to be 1. 
(define fact
  (lambda (x)
    (cond
      [(zero? x) 1]
      [else ( * x (fact (sub1 x)))])))

;; 11.Define and test a procedure memv that takes an element and a list and returns the first 
;; cdr whose car is eqv? to the element, or #f if the element is absent from the list.
(define memv
  (lambda (x ls)
    (cond 
      [(null? ls) #f]
      [(eqv? x (car ls)) ls]
      [else (memv x (cdr ls))])))

;;12. Define and test a procedure fib that takes a natural number n as input 
;;and computes the nth number, starting from zero, in the Fibonacci sequence
(define fib
  (lambda (x)
    (cond
      [(zero? x) 0]
      [(<= x 2) 1]
      [else (+ (fib (- x 1)) (fib ( - x 2)))])))

;;13. The expressions (a b) and (a . (b . ())) are equivalent. 
;;Using this knowledge, rewrite the expression ((w x) y (z)) using as many 
;;dots as possible. Be sure to test your solution using Racket's equal?
;;predicate.
;;'( (w . (x . () ) ) y (z . ()))

;;14. Define and test a procedure binary->natural that takes a flat list 
;;of 0s and 1s representing an unsigned binary number in reverse bit order 
;;and returns that number. 
(define binary->natural 
  (lambda (ls)
    (define helper
      (lambda (ls temp)
        (cond
          [(null? ls) 0]
          [(zero? (car ls)) (helper (cdr ls) (+ 1 temp))]
          [else (+ (expt 2 temp) (helper (cdr ls) (add1 temp)))])))
    (helper ls 0)))
;;15. Define subtraction using natural recursion. Your subtraction function, 
;;minus, need only take nonnegative inputs where the result will be nonnegative. 
(define minus
  (lambda (x y)
    (cond
      [(zero? y) x]
      [else (sub1 (minus x (sub1 y)))])))
;;16. Define division using natural recursion. Your division function, div, 
;;need only work when the second number evenly divides the first.
;;Division by zero is of course bad data. 
(define div
  (lambda (x y)
     (cond
       [(zero? x) 0]
       [else (add1 (div (minus x y) y))])))
;;17. Define a function append-map that, similar to map, takes both a procedure
;;p of one argument a list of inputs ls and applies p to each of the elements 
;;of ls. Here, though, we mandate that the result of p on each element of ls is
;;a list, and we append together the intermediate results. 
(define append-map
  (lambda (proc ls)
    (cond
      [(null? ls) '()]
      [else (append (proc (car ls)) (append-map proc (cdr ls)))])))
;;18. Define a function set-difference that takes two flat sets (lists with no
;;duplicate elements) s1 and s2 and returns a list containing all the elements 
;;in s1 that are not in s2. 
(define set-difference
  (lambda (ls1 ls2)
    (define member?
      (lambda (x ls)
        (cond
          [(null? ls) #f]
          [(equal? x (car ls)) #t]
          [else (member? x (cdr ls))])))
    (cond
      [(null? ls1)'()]
      [(null? ls2) ls1]
      [(member? (car ls1) ls2) (set-difference (cdr ls1) ls2)]
      [else (cons (car ls1) (set-difference (cdr ls1) ls2))])))
;;19. In mathematics, the power set of any set S, denoted P(S), is the set of 
;;all subsets of S, including the empty set and S itself. 
(define powerset
  (lambda (ls)
    (cond
      [(null? ls) '(())]
      [else (append-map (lambda (x) (list x (cons (car ls) x)))
       (powerset (cdr ls)))])))