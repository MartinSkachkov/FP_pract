#lang racket

;-----------------------------------------------------
;High-order functions - функция, която приема функция за параметър
(define (fixed-point? func x)
  (= (func x) x))

(fixed-point? sin 0)

(define (id x)
  x)

(id id) ; returns #<procedure:id>
(id (id id))

;k^2 + (k +1)^2 +...+ 100^2 3a.k <= 100
(define (sum1 k)
  (cond
    [(> k 100) 0] ;base case
    [else (+ (* k k) (sum1 (+ k 1)))])) ;tracking back the recursion

(sum1 3)

;някви интеграли xD
(define (sum2 a b f dx)
 (cond
   [(> a b) 0]
   [else (+ (* dx (f a)) (sum2 (+ a dx) b f dx))]))

;може да забележим че функциите sum1 и sum2 имат доста общи работи.
;base case-a е че дадено нещо е по-голямо от друго нещо и връшаме в двата случая 0.
;в else case-a събираме някакъв резултат с рекурсивно извикване на функцията (натрупване на резултата).
;правим рекурсивно извикване на фунцкията за следващата стъпка.
;разликата е само по начина, по който смятаме чисто математически нещата.

(define (square x) (* x x))
(define (1+ x) (+ x 1))

;abstract function for multiple purposes
(define (sum a b term next) ; term & next са функционални обекти
  (cond
    [(> a b) 0]
    [else (+ (term a) (sum (term next) b term next))]))

(define (sum1-altered k)
  (sum k 100 square 1+))
(sum1-altered 3)