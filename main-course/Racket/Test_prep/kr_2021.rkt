#lang racket

;Задача 1.
;Да се дефинира функция (get-distribution n), която приема неотрицателно
;цяло число n и връща списък с елементи от вида (<цифра> . <брой срещания
;в n^2>), представящ разпределението на цифрите на n^2. Резултатът да е
;сортиран спрямо цифрите на n^2.

(define (remove-duplicates subxs xs)
  (define (helper subys ys)
    (cond
      [(null? subys) (remove-duplicates subxs ys)]
      [(null? ys) xs]
      [(= (car subys) (car ys)) (helper (cdr subys) (cdr ys))]
      [else (cons (car xs) (remove-duplicates subxs (cdr xs)))]))
  (helper subxs xs))
(remove-duplicates '(2) '(1 2 1 2 1))   ; -> '(1)

(define (number->list num)
  (let loop ((num num)
             (acc '()))
    (if (< num 10)
        (cons num acc)
        (loop (quotient num 10)
              (cons (remainder num 10) acc)))))
;(number->list 1234)

(define (count-repetitions pred currNumlst)
  (define (counter currNumlst c)
    (cond
      [(null? currNumlst) c]
      [(= pred (car currNumlst)) (counter (cdr currNumlst) (+ c 1))]
      [else (counter (cdr currNumlst) c)]))
  (counter currNumlst 0))
;(count-repetitions 3 (number->list 1234))

;15129
(define (get-distribution n)
  (define n^2 (expt n 2))
  (define (helper currNumlst)
    (cond
      [(null? currNumlst) '()]
      [else (cons (cons (car currNumlst) (count-repetitions (car currNumlst) currNumlst)) (helper (remove-duplicates (list (car currNumlst)) currNumlst)))]))
  (helper (number->list n^2)))
(get-distribution 123)

;Задача 2.
;Даден е списък от непразни списъци. Елементите на дадения списък са такива, че
;ако сортирате списъка от техните дължини, ще получите списък с естествени числа.
;Те биха формирали строго нарастваща редица от поредни естествени числа, в
;която едно число липсва.
;Да се дефинира функция (get-missing-length xss), която връща липсващото
;число за списъка xss. Ако някой от елементите на xss е празен или списъкът xss
;е празен, да се връща грешката “Empty list!”.

(define (create-len-lst lst)
  (cond
    [(null? lst) '()]
    [(null? (car lst)) "Empty list"]
    [else (cons (length (car lst)) (create-len-lst (cdr lst)))]))
;(sort (create-len-lst  '((1 2) (4 5 1 1) (1) (5 6 7 8 9))) <)

(define (get-missing-len xs)
  (define (find-missing lst missing)
    (cond
      [(= (car lst) missing) (find-missing (cdr lst) (+ missing 1))]
      [else missing]))
  (if (null? xs)
      "Empty list"
      (find-missing (sort (create-len-lst xs) <) 1)))

(get-missing-len '((1 2) (4 5 1 1) (1) (5 6 7 8 9)))
(get-missing-len '(("a", "a", "a") ("a", "a") ("a", "a", "a",
"a") ("a") ("a", "a", "a", "a", "a", "a")))
;(get-missing-len '())
;(get-missing-len '(() (123)))

;Задача 3.
;Да се дефинира процедура от по-висок ред (trailing-zeros n), която връща
;анонимна функция, която приема предикат p и проверява дали p е в сила за броя
;на влачещите нули в n!. Вместо да се пресмята факториелът, да се използва
;следното свойство: броят на влачещите нули в n! е равен на
;където естественото число k пробягва интервала от 1 до най-високата степен на
;5, която не надвишава n.
;Например броят на влачещите нули в 127! е 31

(define (calculate-sum n)
  (define (process k res)
    (cond
      [(< n (expt 5 k)) res]
      [else (process (+ k 1) (+ res (floor (/ n (expt 5 k)))))]))
  (process 1 0))

(define (trailing-zeros n)
  (λ (p) (p (calculate-sum n))))

((trailing-zeros 6) even?)
((trailing-zeros 1000) even?)
((trailing-zeros 100000) even?)
((trailing-zeros 1000000000) even?)

;Задача 4.
;Да се дефинира функция (persistence n), която приема естествено число n и
;връща точкова двойка от вида ‘(ys . x). Първият елемент на резултата е списък,
;чийто първи елемент е равен на произведението на цифрите на n, а всеки следващ
;е равен на произведението на цифрите на предходния до получаването на
;едноцифрено произведение, на което е равен последният елемент на ys. Вторият
;елемент на резултата (т.е. x) е равен на дължината на списъка ys.

(define (len num)
    (cond
      [(= num 0) 0]
      [else (+ 1 (len (quotient num 10)))]))
;(len 1234)

(define (digit-multiplication n)
  (cond
    [(= n 0) 1]
    [(* (remainder n 10) (digit-multiplication (quotient n 10)))]))
;(digit-multiplication 123)

(define (persistence n)
  (define (construct-ys num)
    (cond
      [(< num 10) '()]
      [else (cons (digit-multiplication num) (construct-ys (digit-multiplication num)))]))
  (if (< n 10)
      (cons n 1)
      (cons (construct-ys n) (length (construct-ys n)))))

(persistence 39)
(persistence 126)
(persistence 4)
(persistence 999) 






