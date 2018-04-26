#lang racket

(define (myfn x y) (+ (* x x) y))



(define (fold-tree fn init tree)
  (cond
    [(empty? tree) init]
    [else (fn (first tree) (fold-tree fn init (second tree)) (fold-tree fn init (third tree)))]))

(define testree
  (list " how"
        (list " my" (list "hey" empty empty)
              (list " man" empty empty))
        (list " is" empty (list " you" empty empty))
        ))

(define (sum-list lst)
  (cond
    [(empty? lst) 0]
    [else (+ (first lst) (sum-list (rest lst)))]))

(define (prod-list lst)
  (cond
    [(empty? lst) 1]
    [else (* (first lst) (prod-list (rest lst)))]))

(define (my-map fn lst)
  (my-foldr (λ (f r)
             (cons (fn f) r)) empty lst))

(define (my-filter fn lst)
  (my-foldr (lambda (f r)
              (cond
                [(fn f) (cons f r)]
                [else r])) empty lst))



;; Syntax review:

;; Overall syntax:
;;(function/operation arg1 arg2 ....)

;; Function definitions:
;; (define (function_name arg1 arg2 ...)
;;    ([implementation of function]))

;; Conditionals:
;; (cond
;;   ((condition1) (implementation1))
;;   [(condition2) (implementation2)]
;;    ...
;;   [else (implementation)] ;;Note: else is optional, but recommended

;; Lambdas:
;; (lambda (arg1 arg2 ...) ([implementation of lambda]))
;; Note, we can also write λ (cmd + \), and so e.g. (λ (arg1 arg 2) (+ arg1 arg2)) works 


;; Some built in functions:
;; +, *, >, <, >=, <=, =, sqrt, or, and, not
;; string? number? list? boolean? equal? empty?
;; string-append, string-length,
;; cons -> append element onto front of list, i.e. (cons 3 empty) = (list 3) = '(3), (cons 3 (list 1 5 7)) = (list 3 1 5 7) = '(3 1 5 7)
;; append (for appending two or more lists together), length (of list), reverse
;; foldr, map, filter

;; Note that "and" is used as expected, e.g.
;; (and #t #t #t) -> #t
;; (and #t #f) -> #f
;; (and (> 3 0) (< 2 3) (= 1 (- 3 2))) -> #t
;; (and (< 1 7) (> 5 5)) -> #f

;; andmap -> given a list, lst, and a predicate function, fn, returns true if
;;  all elements of lst satisfy fn, i.e. if all elements of lst, when passed into fn, return true
;;  and returns false otherwise
;;  Note: base case ("init" argument in foldr) is difficult to understand.


(define (my-andmap fn lst)
  (foldr (lambda (f r)
           (and (fn f) r)) #t lst))

;; Examples:
(my-andmap string? '("Hello" "my" "man")) 
(my-andmap string? '(3 "Hello" "my" "man")) 
(my-andmap string? '("Hello" 3 "my" "man")) 
(my-andmap string? '("Hello" "my" "3" "man"))


(define (my-ormap fn lst)
  (foldr (lambda (f r)
           (or (fn f) r)) #f lst))

(my-ormap string? '("Hello" "my" "man")) 
(my-ormap string? '(3 "Hello" "my" "man")) 
(my-ormap string? '("Hello" 3 "my" "man")) 
(my-ormap string? '("Hello" "my" "3" "man"))  






;; ********************************************************************************************************************


;; inside: fold -> (fn base lst) -> X ;; IMPLICITLY fold is also an argument to the lambda, so:
;; inside: fold -> (fn base lst fold) -> X ;; fold was put last so that the simplest arguments could be first
(define (inside fold)
  (λ (fn base lst)
        (cond
          [(empty? lst) base]
          [else (fn (first lst) ((fold fold) fn base (rest lst)))])))



;; recurser: fold -> (inside fold)
;; = fold -> ((fold -> (fn base lst fold) -> X)  fold)
;; = fold -> ((fn base lst fold) -> X)

(define recurser
   (λ (fold) (inside fold)))



;; applier: g -> (g g)

(define applier 
  (λ (g) (g g)))



;;(applier recurser) = (recurser recurser) = ( [fold -> ((fn base lst fold) -> X)] [fold -> ((fn base lst fold) -> X)] )
;; = ((fn base lst [fold -> ((fn base lst fold) -> X)] ) -> X)

;; What happens when fold is applied onto itself? (This happens in the inside as (fold fold)). Note fold is in square brackets directly above, the last argument to our overall function
;; ([fold -> ((fn base lst fold) -> X)] [fold -> ((fn base lst fold) -> X)])
;; = ((fn base lst [fold -> ((fn base lst fold) -> X)] ) -> X)
;; Again, one achieves the same function. This is why the recursion works.

;; Last, we give fn, base, and lst as arguments to (applier recurser), so:
;; ((applier recurser) fn base lst)
;; = ({(fn base lst [fold -> ((fn base lst fold) -> X)] ) -> X)} fn base lst) ;; Note, the fourth argument is created by us, so it is not provided
;; = X
;; as required. 
(define (foldr3 fn base lst)
  ((applier recurser) fn base lst))

;;Examples:
;;(foldr3 + 0 '(3)) ;;works with (fold x) where x is ANY expression, because no second recursive call is needed 
;;(foldr3 * 1 '(7)) ;;same as above
;;(foldr3 + 5 '(1 2 3))



(define (Umer-andmap fn lst)
  (my-foldr (λ (f r) (and (fn f) r)) #t lst))

(define (sum-lst lon) (foldr + 0 lon))


;;first: listof X -> X
;; rest: listof X -> listof X

(define (my-map2 fn lst)
  (cond
    [(empty? lst) empty]
    [else (cons (fn (first lst)) (my-map2 fn (rest lst)))]))


(define (Fib n listFib)
  (cond
    [( = n 0) (list 0)]
    [( = n 1) (list 1 0)]
    [else
      (define lstn-1 (Fib (- n 1) listFib))
      (list (+ (car lstn-1) (cadr lstn-1)) (car lstn-1))]))

(define (Fibonacci n)
  (car (Fib n empty)))

;; my-foldr: (X Y -> Y) Y (listof X) -> Y

(define (my-foldr fn base lst)
  (cond
    [(empty? lst) base]
    [else (fn (first lst) (my-foldr fn base (rest lst)))]))

;; sum-string: ((string num) -> num ) num (listof string) -> num

;; curry: (X Y -> Z) -> (X -> (Y -> Z))
(define (curry f)
  (lambda (x) (lambda (y) (f x y))))

;; uncurry: (X -> (Y -> Z)) -> (X Y -> Z)
(define (uncurry g)
  (lambda (x y) ((g x) y)))


;; compose: (Y -> Z) (X -> Y) -> (X -> Z)
(define (compose f g)
  (lambda (x) (f (g x))))

;;string-sum takes in a list of strings and produces the sum of the lengths of the strings
(define (string-sum lst) (foldr (λ (x y) (+ (string-length x) y)) 0 lst))

;;write it without lambdas, using only curry, uncurry, and compose
(define (string-sum2 lst)
  (foldr (uncurry (compose (curry +) string-length)) 0 lst))

;; why does this work??
;; string-length: String -> Num
;; +: Num Num -> Num
;; curry: (A B -> C) -> (A -> (B -> C))
;; (curry +): (Num -> (Num -> Num))
;; compose: (B -> C) (A -> B) -> A -> C
;; (compose (curry +) string-length): (compose (Num -> (Num -> Num) (String -> Num)) = String -> (Num -> Num)
;; uncurry: (X -> (Y -> Z)) -> (X Y -> Z)
;; (uncurry (compose (curry +) string-length)): (uncurry String -> (Num -> Num)) = String Num -> Num


;; Exercise: Write filter without using explicit recursion, i.e. using foldr
;; Exercise: Write map without explicit lambdas or explicit recursion, using curry, uncurry, and compose
;; Exrcise: Write filter without using explict lambdas or explicit recursion as above

(define (map2 fn lst)
  (foldr (uncurry (compose (curry cons) fn)) empty lst))

(define (filter2 fn lst)
  (foldr (lambda (x y)
           (cond
             [(fn x) (cons x y)]
             [else y]))
         empty lst))

(define (test0 a)
  (if (> a 0) 7 -7))

(define (my-if x y z)
  (if x y z))

(define (curry3 f)
  (lambda (x)
    (lambda (f)
      (lambda (z)
        (f x z)))))

(define (test a)
  ((((curry3 my-if) (> a 0)) 7) -7))

(define (badfib n)
  (cond
    [(= n 0) 0]
    [(= n 1) 1]
    [else (+ (badfib (- n 1)) (badfib (- n 2)))]))




       