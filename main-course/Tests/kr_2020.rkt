#lang racket

;Задача 1. Някои числа имат интересни свойства. Например:
;89 → 81 + 92 = 89 * 1
;695 → 62 + 93 + 54 = 1390 = 695 * 2
;46288 → 43 + 64 + 25 + 86 + 87 = 2360688 = 46288 * 51
;Да се дефинира процедура (dig-pow n p), която приема естествено число n
;(записано с цифри abcd…, които могат да се повтарят) и намира естествено число
;k – такова, че (ap + bp+1 + cp+2 + dp+3 + ...) = n*k. Ако число k с посоченото
;свойство не съществува, да се връща -1.

(define (len num)
    (cond
      [(= num 0) 0]
      [else (+ 1 (len (quotient num 10)))]))

(define (dig-pow n p)
  (define (helper num pow 10s)
    (cond
      [(= num 0) 0]
      [else (+ (expt (quotient num 10s) pow) (helper (remainder num 10s) (+ pow 1) (/ 10s 10)))]))
  (if (< (/ (helper n p (expt 10 (- (len n) 1))) n) 0.999)
      -1
      (/ (helper n p (expt 10 (- (len n) 1))) n)))

(dig-pow 89 1)
(dig-pow 92 1)
(dig-pow 695 2)
(dig-pow 46288 3)

;Задача 2.
;Даден е списък от непразни списъци. Елементите на дадения списък са такива, че
;ако сортирате списъка от техните дължини, ще получите списък с естествени числа.
;Те биха формирали строго нарастваща редица от поредни естествени числа, в
;която едно число липсва.
;Да се дефинира функция (get-missing-length xss), която връща липсващото
;число за списъка xss. Ако някой от елементите на xss е празен или списъкът xss
;е празен, да се връща грешката “Empty list!”.

(define (create-negative-lst lst)
  (cond
    [(null? lst) '()]
    [(< (car lst) 0) (cons (car lst) (create-negative-lst (cdr lst)))]
    [else (create-negative-lst (cdr lst))]))

(define (remove-duplicates lst)
  (cond
    [(empty? lst) empty]
    [(member (first lst) (cdr lst)) 
     (remove-duplicates (rest lst))]
    [else (cons (first lst) (remove-duplicates (rest lst)))]))

(define (kth-max-min xs)
  (λ (k)
       (define (helper lst k)
             (cond
               [(> k (length lst)) "No such number!"]
               [(= k 1) (car lst)]
               [else (helper (cdr lst) (- k 1))]))
           (helper (remove-duplicates (sort (create-negative-lst xs) >)) k)))
(sort (create-negative-lst '(-1 0 -1 0 -2 3 1 -1)) >)

((kth-max-min '(1 2 3 4 -5 6 7 -2 -1 0)) 2)
((kth-max-min '(-1 0 -1 0 -2 3 1 -1)) 3)

;Задача 3. Да се дефинира процедура (shuffle xs), която получава списък от
;2*n елемента във вида '(x1 x2 ... xn y1 y2 ... yn) и връща списък във вида
;'(x1 y1 x2 y2 ... xn yn).

(define (create-xs lst)
  (define (helper currLst n)
    (cond
      [(= n (/ (length lst) 2)) '()]
      [else (cons (car currLst) (helper (cdr currLst) (+ n 1)))]))
  (helper lst 0))

(define (create-ys lst)
  (define (helper currLst n)
    (cond
      [(= n (/ (length lst) 2)) currLst]
      [else (helper (cdr currLst) (+ n 1))]))
  (helper lst 0))
      

(define (shuffle xs)
  (define (generate-res lst1 lst2)
    (cond
      [(null? lst1) '()]
      [else (append (list  (car lst1) (car lst2)) (generate-res (cdr lst1) (cdr lst2)))]))
  (generate-res (create-xs xs) (create-ys xs)))

(shuffle '(2 5 1 3 4 7))
(shuffle '(1 2 3 4 4 3 2 1))
(shuffle '(1 1 2 2))

;Задача 4. Да се дефинира предикат (triangular? mat), който получава
;квадратна числова матрица, представена като списък от списъци, и проверява дали
;тя е горно триъгълна, т.е. дали всички елементи под главния ѝ диагонал са нули.

(define (number-of-0s lst)
  (define (count lst zeroes)
    (cond
      [(null? lst) zeroes]
      [(not (zero? (car lst))) zeroes]
      [else (count (cdr lst) (+ zeroes 1))]))
  (count lst 0))

(define (triangular? matrix)
  (define (helper lst zero-count)
    (cond
      [(null? lst) #t]
      [(equal? (number-of-0s (car lst)) zero-count) (helper (cdr lst) (+ zero-count 1))]
      [else #f]))
  (helper matrix 0))


 (triangular?  (list (list 1 0 0)
                     (list 0 1 1)
                     (list 0 0 1)))
(triangular? '((1 2 3 4)
               (0 5 6 7)
               (0 0 8 9)
               (0 0 0 9)))
(triangular? '((1 2 3)
               (1 5 6)
               (0 0 9))) 