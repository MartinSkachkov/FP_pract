#lang racket

;task 1
(define fs (list *
 (λ (x y) (* x x x y))
 (λ (x y) (+ x 1 y))
 (λ (x y) (- x (+ 1 y)))
 (λ (x y) (* x y 2))))

(define xs '(1 2 3 4 5))

(define (id)
  (λ (x y) x))

(define (calc-comp f1 f2)
  (λ (x1 x2 y) (f1 x1 (f2 x2 y))))

(define (call-from-len fs xs y)
  (cond
    [(odd? (length fs)) (process-odd fs xs y)]
    [(even? (length fs)) (process-even fs xs y)]))

(define (process-even fs xs y)
  (cond
    [(null? fs) 0]
    [else (+((calc-comp (car fs) (cadr fs)) (car xs) (cadr xs) y)  (process-even (cddr fs) (cddr xs) y))]))

(define (process-odd fs xs y)
  (cond
    [(null? (cdr fs)) ((calc-comp (car fs) (id)) (car xs) y y)]
    [else (+((calc-comp (car fs) (cadr fs)) (car xs) (cadr xs) y)  (process-odd (cddr fs) (cddr xs) y))]))

(define (pair-compose fs xs)
  (λ (y)
    (call-from-len fs xs y)))

((pair-compose fs xs) 5)

;task 2
(define (point-is-in-interval p low up)
  (cond
    [(and (>= p low) (<= p up)) #t]
    [else #f]))

(define (woodcutters xs)
  (define (counter lst1 lst2 c) 
    (cond
      [(null? (cdr lst2)) (+ c 1)] ;последното дърво ще пада винаги надясно
      [(not (point-is-in-interval (car (car lst1)) (- (car (car lst2)) (cdr (car lst2))) (car (car lst2)))) (counter (cdr lst1) (cdr lst2) (+ c 1))]
      [(not (point-is-in-interval (car (cadr lst2)) (car (car lst2)) (+ (car (car lst2)) (cdr (car lst2))))) (counter (cdr lst1) (cdr lst2) (+ c 1))]
      [else (counter (cdr lst1) (cdr lst2) c)]))
 (cond
   [(null? xs) 0]
   [(null? (cdr xs)) 1]
   [else (counter xs (cdr xs) 1)])) ;първото е паднало наляво

(woodcutters '((1 . 2) (2 . 1) (5 . 10) (10 . 9) (19 . 1)))
(woodcutters '((1 . 2) (2 . 1) (5 . 10) (10 . 9) (20 . 1)))
(woodcutters '((10 . 4) (15 . 1) (19 . 3) (20 . 1)))
(woodcutters '((1 . 7) (3 . 11) (6 . 12) (7 . 6) (8 . 5) (9 . 11)
 (15 . 3) (16 . 10) (22 . 6) (23 . 3) (25 . 7) (27 . 3) (34 . 5)
 (35 . 10) (37 . 3) (39 . 4) (40 . 5) (41 . 1) (44 . 1) (47 . 7)
 (48 . 11) (50 . 6) (52 . 5) (57 . 2) (58 . 7) (60 . 4) (62 . 1)
 (67 . 3) (68 . 12) (69 . 8) (70 . 1) (71 . 5) (72 . 5)
 (73 . 6) (74 . 4) ) ) 
