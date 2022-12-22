#lang racket

(define x 5) ;defined a variable which value is 5
x

(define (f) 3) ;defined a function without args which returns 3
(f)

;Задача 1. Да се напише функция mymin, която приема два аргумента и връща по-малкия от тях.
(define (my-min arg1 arg2)
  (cond
    [(> arg1 arg2) arg1]
    [else (< arg1 arg2) arg2]))

(my-min 1 2)
(my-min 5 3)

(define (my-min-if arg1 arg2)
  (if (> arg1 arg2)
      arg1
      arg2))

(my-min-if 1 2)
(my-min-if 5 3)

;Задача 2. Да се дефинира функцията inside? x a b, която проверява дали числото x се намира в затворения интервал [a, b].
(define (inside? x a b)
  (cond
    [(and (>= x a) (<= x b)) #t]
    [else #f]))

(inside? 3 3 5)
(inside? 1 3 5)
(inside? 4 3 5)

;Задача 3. Да се напише функция myfunc, която пресмята средно аритметично на квадратите на 2 числа.
(define (my-func x y)
  (define (expn num)
    (* num num))
  (/ (+ (expn x) (expn y)) 2))

(my-func 5 5)
(my-func 8 12)
;(expn 5) inaccessible (only accessible in the function definition my-func

;Задача 4. Да се напише myfib, която получава един аргумент n и връща n-тото число на Фибоначи. 
;Да се напише и итеративно решение.
(define (my-fib n)
  (cond
    [(= n 0) 0]
    [(= n 1) 1]
    [else (+ (my-fib (- n 1)) (my-fib (- n 2)))]))

(my-fib 3)

;linearly recursive
(define (fib-iter n)
  (define (helper prev cur i)
    (if (= i n)
        cur
        (helper cur (+ prev cur) (+ i 1))))
  (helper 1 0 0))

; (fib-iter 4) -> (helper 1 0 0) -> (helper 0 1 1) -> (helper 1 1 2)
; -> (helper 1 2 3) -> (helper 2 3 4) -> 3

(fib-iter 0)
(fib-iter 1)
(fib-iter 2)
(fib-iter 3)
(fib-iter 7)

;Задача 5. Рекурсивен факториел
(define (my-fact n)
  (cond
    [(= n 0) 1]
    [else (* n (my-fact (- n 1)))]))

(my-fact 5)
