#lang racket

(define (g-l-sum limit)
  (define (find x y)
    (cond
      [(= y 0) (find (- x 1) limit)]
      [(= (+ (gcd x y) (lcm x y)) limit) (cons x y)]
      [else (find x (- y 1))]))
  (find limit limit))

(g-l-sum 2) ; → '(1 . 1)
(g-l-sum 14) ; спира на първото такова срещане
(g-l-sum 5)

#|
това извежда нещата от пример, но с рестрикция
(define (g-l-sum limit)
  (define (find x y)
    (cond
      [(= y (- limit 3)) (find (+ x 1) 0)]
      [(= (+ (gcd x y) (lcm x y)) limit) (cons x y)]
      [else (find x (+ y 1))]))
  (find 1 1))|#