#lang racket

;Задача 1. Да се дефинира функция (sum-numbers a b), приемаща два аргумента, която
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

(define (switchsum f g n)
  (λ (x)
    (cond
      [(= n 0) 0]
      [else (+ (f x) ((switchsum g f (- n 1)) (f x)))])))

;Задача 4. Да се дефинира функция (repeater str), която получава като аргумент символен
;низ и връща анонимна функция на два аргумента - count и glue (число и низ). Оценката на
;обръщението към върнатата функция е низ, който се получава чрез count-кратно повтаряне
;на низа str, при което между всеки две съседни повторения на str стои низът glue.

(define (repeater str)
  (λ (count glue)
    (define (helper res count)
      (cond
        [(= count 1) (string-append  res str)]
        [else (helper (string-append res str glue) (- count 1))]))
    (helper "" count)))
        
((repeater "I love Racket") 3 " ")
((repeater "Quack") 5 "!")

;Задача 5. Да се дефинира функция (sum-sum-digit a b k), която намира сумата на
;естествените числа от a до b (0<a≤b), сумата от цифрите на които е кратна на k.

(define (is-div-by-k currNum k)
  (cond
    [(= currNum 0) #t]
    [(= (remainder currNum k) 0) (is-div-by-k (quotient currNum 10) k)]
    [else #f]))

(define (sum-digits currNum)
  (define (helper currNum res)
    (cond
      [(= currNum 0) res]
      [else (helper (quotient currNum 10) (+ res (remainder currNum 10)))]))
  (helper currNum 0))

(define (sum-sum-digit a b k)
  (define (calculate currNum res)
    (cond
      [(> currNum b) res]
      [(is-div-by-k (sum-digits currNum) k) (calculate (+ currNum 1) (+ res currNum))]
      [else (calculate (+ currNum 1) res)]))
  (calculate a 0))

(sum-sum-digit 3 6 3)

;Задача 6. Да се дефинира функция (max-ordered-sublist lst), която намира найдългия възходящо сортиран подсписък на списъка от числа lst

(define (sublist xs)
  (cond
    [(null? xs) '()]
    [(or (null? (cdr xs)) (> (car xs) (cadr xs))) (list (car xs))] 
    [else (cons (car xs) (sublist (cdr xs)))]))
    
(define (max-ordered-sublist lst)
  (define (helper currLst currMax)
    (cond
      [(null? currLst) currMax]
      [(> (length (sublist currLst)) (length currMax)) (helper (cdr currLst) (sublist currLst))]
      [else (helper (cdr currLst) currMax)]))
  (helper lst (sublist lst)))

(max-ordered-sublist '(1 5 2 4 6 8 3 4 1)); → '(2 4 6 8)

;Задача 7. Да се дефинира функция (where list-elements list-predicates), която
;връща списък от всички елементи на list-elements, за които са изпълнени всички
;предикати в list-predicates.

(define (where list-elements list-predicates)
  (cond
    [(null? list-elements) '()]
    [(and ((car list-predicates) (car list-elements)) ((cadr list-predicates) (car list-elements))) (cons (car list-elements) (where (cdr list-elements) list-predicates))]
    [else (where (cdr list-elements) list-predicates)]))
  
(where '(3 4 5 6 7 8 9 10) (list even? (lambda (x) (> x 5))))
(where '(3 4 5 7) (list even? (lambda (x) (> x 5))))

;Задача 8. Да се дефинира функция (set-union xs ys), която връща обединението на
;множествата от числа xs и ys, представени като списъци, наредени във възходящ ред.
;Елементите на резултантното множество също трябва да са наредени във възходящ ред.

(define (set-union xs ys)
  (define (helper currxs currys)
     (cond
       [(null? currxs) (append currys)]
       [(= (car currxs) (car currys)) (cons (car currxs) (helper (cdr currxs) (cdr currys)))]
       [else (cons (car currxs) (helper (cdr currxs) currys))]))
  (define (helper2 currxs currys)
     (cond
       [(null? currys) (append currxs)]
       [(= (car currxs) (car currys)) (cons (car currys) (helper2 (cdr currxs) (cdr currys)))]
       [else (cons (car currys) (helper2 currxs (cdr currys)))]))

(if (< (car xs) (car ys))
     (helper xs ys)
     (helper2 xs ys)))

(set-union '(1 3 5 7) '(5 7 13)) ;→ '(1 3 5 7 13)
(set-union '(5 7 13) '(1 3 5 7)); → '(1 3 5 7 13)