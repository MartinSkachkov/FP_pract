#lang racket

;намира ни на кой ред е единицата
(define (find-1 matrix)
  (define (find-row-num matrix row)
    (cond
      [(null? matrix) row]
      [(member 1 (car matrix)) row]
      [else (find-row-num (cdr matrix) (+ row 1))]))
  (find-row-num matrix 1))

;трябва да транспонираме матрицата и после то ще си преброи колко реда трябва да switchne и ще изкара резултата
;(define (matrix-transpose matrix)
  ;(define (helper matrix n)
  ;  (cond
   ;   [(= n 5) '()]
   ;   [

(define (steps-bm xss)
  (define (helper xss i j)
    (cond
      [(= (caddr (caddr xss)) 1) (+ i j)]
      [(not (equal? (find-1 xss) 3)) (+ i 1)]
      [(not (equal? (find-1 (matrix-transpose xss)) 3)) (+ j 1)]))
    (helper xss 0 0))
      


(steps-bm '((0 0 0 0 0)
            (0 0 0 0 1)
            (0 0 0 0 0)
            (0 0 0 0 0)
            (0 0 0 0 0)) )