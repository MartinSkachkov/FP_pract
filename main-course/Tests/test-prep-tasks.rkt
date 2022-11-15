#lang racket

;Да се дефинира функция (sum-numbers a b), приемаща два аргумента, която
;намира сумата на числата в интервала [a,b], чиито цифри са в низходящ (>=) ред.

(define (compare dig1 dig2)
  (cond
    [(>= dig1 dig2) #t]
    [else #f]))

(define (descending-digits? num)
  (define (helper currNum lastDigit)
    (cond
      [(= currNum 0) #t]
      [(not (compare (remainder currNum 10) lastDigit)) #f]
      [else (helper (quotient currNum 10) (remainder currNum 10))]))
  (helper num 0))
  

(define (sum-numbers a b)
  (define (sumation currNum endNum sum)
    (cond
      [(> currNum endNum) sum]
      [else (sumation (+ 1 currNum) endNum (if (descending-digits? currNum)
                                               (+ currNum sum)
                                               sum))]))
  (sumation a b 0))

(sum-numbers 1 9)
(sum-numbers 199 203)
(sum-numbers 219 225)

;Задача 2. Да се дефинира функция (num-bigger-elements lst), която за даден списък от
;числа lst връща като резултат списък с елементи от вида (lsti ni), където lsti е i-тият
;елемент на lst, а ni е броят на елементите на lst, които са по-големи от lsti.

(define (compare1 currNum lst)
  (define (counter trimmedlist c)
    (cond
      [(null? trimmedlist) c]
      [(< currNum (car trimmedlist)) (counter (cdr trimmedlist) (+ c 1))]
      [else (counter (cdr trimmedlist) c)]))
  (counter lst 0))

(define (num-bigger-elements lst)
  (define (helper trimmedlist)
  (cond
    [(null? trimmedlist) '()]
    [else (cons (cons (car trimmedlist) (compare1 (car trimmedlist) lst)) (helper (cdr trimmedlist)))]))
  (helper lst))

(num-bigger-elements '(5 6 3 4))
(num-bigger-elements '(1 1 1))

;Задача 3. Ако f и g са числови функции и n е естествено число, да се дефинира функция от повисок ред (switchsum f g n), която връща като резултат функция, чиято стойност в дадена
;точка x е равна на f(x)+g(f(x))+f(g(f(x)))+ ... (сумата включва n събираеми).

(define (compose-two f g x)
  (λ (x) (f (g x))))

(define (switchsum f g n)
  (λ (x) (define (sum s n) 
           (cond
             [(= n 0) (+ s (f x))]
             [else (sum (+ s compose-two f g x) (- n 1))]))))