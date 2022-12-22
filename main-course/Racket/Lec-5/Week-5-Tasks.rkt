#lang racket

;Задача 1. Да се дефинира функция, която намира дължината на списък.
(define (list-length xs)
   (cond
     [(null? xs) 0]
     [else (+ 1 (list-length (cdr xs)))]))

(list-length '(1 2 3 4))
(list-length (list 1 2 3 4))
(list-length (cons 1 (cons 2 '())))

;Задача 2. Да се дефинира функция, която проверява дали даден елемент се съдържа в списък.
(define (find x xs)
  (cond
    [(null? xs) #f]
    [(equal? (car xs) x) #t]
    [else (find x (cdr xs))]))

(find 4 '(1 2 3 4 5))
(find 7 '(1 2 3 4 5))
(find '(1 2) '(1 2 3 4 5))
(find '(1 2) '(11 10 (1 2) 4 5))
(find '(1 2) '(11 10 ((1 2) 7) 4 5))

;Задача 3. Да се дефинира функция, която добавя елемент на зададена позиция в списък.
(define (add-elem xs elem pos)
  (define (helper i xs)
    (cond
      [(null? xs) (list elem)]
      [(= i pos) (cons elem xs)]
      [else (cons (car xs) (helper (+ i 1) (cdr xs)))]))
  (helper 0 xs))

(define (insert-at elem pos xs)
  (cond
    [(null? xs) (list elem)]
    [(= pos 0) (cons elem xs)]
    [else (cons (car xs) (insert-at elem (- pos 1) (cdr xs)))]))

(add-elem '(0 1 2 3 5 6 7) 4 4)
(add-elem '() 3 10)

(insert-at 4 4 '(0 1 2 3 5 6 7))
(insert-at 3 10 '())

;Задача 4. Да се дефинира функция, която намира най-малкия елемент на списък.
(define (compare x y)
  (cond
    [(<= x y) x]
    [else y]))

(define (min-elem xs)
  (cond
    [(null? xs) 0])
  (define (find min xs)
    (cond
      [(null? xs) min]
      [else (find (compare min (car  xs)) (cdr xs))])) 
  (find 10000000000000000000000000 xs))
(min-elem '(10 2 3 4))

(define (minimum xs)
  (cond
    [(null? xs) 0]
    [(null? (cdr xs)) (car xs)]
    [(min (car xs) (minimum (cdr xs)))]))
(minimum '(1 2 3 4))
(minimum '(10 2 3 4))

;Задача 5. Да се дефинира функция, която изтрива първото срещане на даден елемент в списък.
(define (erase x xs)
  (cond
    [(null? xs) '()]
    [(equal? x (car xs)) (cdr xs)]
    [else (cons (car xs) (erase x (cdr xs)))]))

(erase 3 '(1 2 3 4 3 2))
(erase '(1 2) '(1 2 (1 2) 4 3 2))

;Задача 6. Да се дефинира функция, която изтрива всички срещания на даден елемент на списък.
(define (erase-all x xs)
  (cond
    [(null? xs) '()]
    [(equal? x (car xs)) (erase-all x (cdr xs))]
    [else (cons (car xs) (erase-all x (cdr xs)))]))

(erase-all 3 '(1 2 3 4 3 2))
(erase-all '(1 2) '(1 2 (1 2) 4 3 (1 2) 2))

;Задача 7. Да се напише функция, която конкатенира два списъка.
(define (concat xs xss)
  (cond
    [(and (null? xs) (null? xss)) '()]
    [(null? xs) xss]
    [else (cons (car xs) (concat (cdr xs) xss))]))

(concat '(1 2 3 4) '(5 6 7))
(concat '(1 2) '(3 4))
(concat '(1 2) '())
(append '(1 2) '(3 4) '(5 6))

;Задача 8. Да се напише функция, която обръща даден списък.
(define (reverse-list xs)
  (cond
    [(null? xs) '()]
    [else (append (reverse-list (cdr xs)) (list (car xs)))]))
(reverse-list '(1 2 3 4))

;Задача 9. Функция (remove-duplicates subxs xs), която премахва всички срещания на subxs от списъка xs.
(define (remove-duplicates subxs xs)
  (define (helper subys ys)
    (cond
      [(null? subys) (remove-duplicates subxs ys)]
      [(null? ys) xs]
      [(= (car subys) (car ys)) (helper (cdr subys) (cdr ys))]
      [else (cons (car xs) (remove-duplicates subxs (cdr xs)))]))
  (helper subxs xs))

(remove-duplicates '(1 2) '(1 2 1 2 1))   ; -> '(1)
(remove-duplicates '(2) '(1 2 1 2 1))     ; -> '(1 1 1)
(remove-duplicates '(2 1 2) '(1 2 1 2 1)) ; -> '(1 1)
(remove-duplicates '(2 3) '(1 2 1 2 1))   ; -> '(1 2 1 2 1)

;Задача 10. Функция (sublist-between start end xs), която взима подсписъка на xs между позициите start и end.
(define (sublist-between start end xs)
  (cond
    [(null? xs) '()]
    [(= end start) (cons (car xs) '())]
    [(= start 0) (cons (car xs) (sublist-between start (- end 1) (cdr xs)))]
    [else (sublist-between (- start 1) (- end 1) (cdr xs))]))

(sublist-between 2 5 '(0 1 2 3 4 5 6 7)) ; -> '(2 3 4 5)
(sublist-between 2 3 '(0 1 2 3 4 5 6 7)) ; -> '(2 3)

;Задача 11. Функция (count-occcurrences subxs xs), която връща броя срещания на subxs в xs.
(define (count-occcurrences subxs xs)
  (define n (length subxs))
  (define (helper k xs count)
    (cond [(< k n) count]
          [(equal? subxs (take xs n)) (helper (- k 1) (cdr xs) (+ count 1))]
          [else (helper (- k 1) (cdr xs) count)]))
  (helper (length xs) xs 0))

(count-occcurrences '(1 2) '(1 2 1 2 1))   ; -> 2
(count-occcurrences '(2) '(1 2 1 2 1))     ; -> 2
(count-occcurrences '(2 1 2) '(1 2 1 2 1)) ; -> 1
(count-occcurrences '(1 2 1) '(1 2 1 2 1)) ; -> 2