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