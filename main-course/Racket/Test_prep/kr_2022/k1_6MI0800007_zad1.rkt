#lang racket

(define (is-prime? x)
  (define (helper i x)
    (cond
      [(= i 1) #t]
      [(= (remainder x i) 0) #f]
      [else (helper (- i 1) x)]))
  (cond
    [(= x 1) #t]
    [else (helper (- x 1) x)]))

(define (primes-prod x)
  (define (helper currX res)
    (cond
      [(> currX (floor (sqrt x))) res]
      [else (if (is-prime? currX)
             (helper (+ currX 1) (* res currX))
             (helper (+ currX 1) res))]))
  (helper 1 1))

(primes-prod 12)
(primes-prod 49)
(primes-prod 1200)