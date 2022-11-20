#lang racket

(define (shuffle-merge xs ys)
  (define (merge bigger smaller)
    (cond
      [(null? smaller) bigger]
      [(null? bigger) smaller]
      [else (append (list (car bigger) (car smaller)) (shuffle-merge (cdr bigger) (cdr smaller)))]))
  (merge xs ys))

(shuffle-merge '(1) '())
(shuffle-merge '(3 4 5) '(2))
(shuffle-merge '(3 4 5) '(9 2))
(shuffle-merge '(3 2 8) '(5 6 1 9 11))