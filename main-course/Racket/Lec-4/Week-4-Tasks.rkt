#lang racket

;Зад. 1. Да се дефинира предикат (substr? a b), който проверява дали a е подниз на b, където a и b са естествени числа (например 123 е подниз на 5123783).
(define (check currNum substr)
  (cond
    [(= substr 0) #t]
    [(= (remainder currNum 10) (remainder substr 10)) (check (quotient currNum 10) (quotient substr 10))]
    [else #f]))
    
(define (substr? substr num)
  (define (helper currNum)
    (cond
      [(= currNum 0) #f]
      [else (if (check currNum substr)
                #t
                (helper (quotient currNum 10)))]))
  (helper num))

(substr? 12 512)
(substr? 12 5122)
(substr? 12 51022)

; λ === Ctrl + \
;Зад. 2. Дефинирайте следните функции:
;a). (my-identity x), функцията идентитет: връща каквото и дадете.
(define (my-identity x) x)
(my-identity 3)

;б). (my-compose f g), която връща композицията на функциите f и g.
(define (my-compose f g)
  (lambda (x) (f (g x))))
((my-compose (λ (x) (* x 2)) (λ (x) (+ x 5))) 7)

;в). (my-negate p?), която приема предикат p? и връща предиката (not p?).
(define (my-negate p?)
  (lambda (x) (not (p? x))))
((my-negate odd?) 13)

;г). (my-curry f x), която приема многоаргумента функция f и първи аргумент x и връща функцията получена от частичното прилагане на x върху f. Използвайте вградената функция curry.
(define (my-curry f x)
  (lambda (y z) (f x y z)))
((my-curry (λ (a b c) (+ a (* b c))) 2) 4 5)

;Зад. 3. Да се дефинира процедура от по-висок ред (difference F a b), която по дадени едноаргументна реална функция F и две реални числа a и b намира разликата F(b) - F(a).
(define (difference F a b)
  (- (F a) (F b)))
(define res (difference (lambda (x) (* x 2)) 2 3))
res

;Зад. 4. Чрез използване на lambda израз да се дефинира процедурен обект, който е еквивалентен на f, ако имаме дефиницията (define (f x) (* 2 x)).
(define (f x)
  (* 2 x))
(f 3) ;-> 6

;it's precedure obj:
(define obj (λ (x) (* 2 x)))
(obj 3) ;-> 6

;Зад. 5. Да се дефинира процедура от по-висок ред (derive f eps), която намира първа производна на едноаргументната реална функция f с точност eps.
(define (derive f eps)
  (λ (x) (/ (- (f (+ x eps)) (f x)) eps)))

(define (h x) (* 2 x x))

((derive h 1e-4) 5)
;------------------------------------------------
(define (make-pair a b)
  (λ (x) (if x a b)))

(define (one p)
  (p #t))

(define (two p)
  (p #f))

(define p1 (make-pair 1 2)) ;функционален обект
;(define p1 (λ (x) (if x 1 2)))

(one p1)
;(one (λ (x) (if x 1 2)))
;((λ (x) (if x 1 2)) #t)
;((λ (#t) (if #t 1 2)) #t)
;1

(two p1)
;(one (λ (x) (if x 1 2)))
;((λ (x) (if x 1 2)) #f)
;((λ (#f) (if #f 1 2)) #f)
;2

((two (make-pair 1 odd?)) 13)
;((two (λ (x) (if x 1 odd?))) 13)
;(((λ (x) (if x 1 odd?)) #f) 13)
;(((λ (#f) (if #f 1 odd?)) #f) 13)
;(odd? 13)
;#t