#lang racket

;Задача 1. Да се дефинира функция (sum-prime-divisors n), която намира сумата на всички прости делители на едно число n.
(define (prime? num)
  (define (helper div)
    (cond
      [(= num 1) #t]
      [(= div 1) #t]
      [(= (modulo num div) 0) #f]
      [else (helper (- div 1))]))
  (helper (- num 1)))

(define (sum-prime-divisors n)
  (define (helper div sum)
    (cond
      [(= div 1) sum]
      [else (if (and (prime? div) (= (remainder n div) 0))
                (helper (- div 1) (+ sum div))
                (helper (- div 1) sum))]))
  (helper n 0))

(sum-prime-divisors (* 7 13))

;Задача 2. Да се дефинира функция (pow x n), която генерира линейно рекурсивен процес и намира x на степен n, където x е реално, а n - естествено число.
(define (pow x n)
  (cond
    [(= n 1) x]
    [else (* x (pow x (- n 1)))]))

(pow 2 10)
(pow 3 5)

;линейно итеративен (всикчи рекурсивни извиквания са опашкови понеже нямаме отложена операция)
(define (pow-iter x n)
  (define (helper res n)
    (cond
      [(= n 1) (* res x)]
      [else (helper (* x res) (- n 1))]))
  (helper 1 n))

(pow-iter 2 10)
(pow-iter 3 5)

;Задача 3. Да се дефинира функция (count-оccurences d n), намираща броя на срещанията на дадена цифра d в записа на число n.
(define (wanted-digit? d dig)
  (cond
    [(= d dig) #t]
    [else #f]))

(define (count-occurences d n)
  (define (counter count n)
    (cond
      [(= n 0) count]
      [else (if (wanted-digit? d (remainder n 10))
                (counter (+ count 1) (quotient n 10))
                (counter count (quotient n 10)))]))
  (counter 0 n))

(count-occurences 2 1232221)

;Задача 4. Да се дефинира предикат (ascending? n), който връща истина, ако цифрите на дадено естествено число n са в нарастващ ред от първата към последната.
(define (num-len num)
  (define (helper num count)
    (cond
      [(= num 0) count]
      [else (helper (quotient num 10) (+ count 1))]))
  (helper num 0))

(define (compare dig1 dig2)
  (cond
    [(<= dig1 dig2) #t]
    [else #f]))

(define (ascending? n)
  (define (helper currNum div)
    (cond
      [(= div 10) (if (compare (quotient currNum 10) (remainder currNum 10))
                      #t
                      #f)]
      [else (if (compare (quotient currNum div) (quotient (remainder currNum div) (/ div 10)))
                (helper (remainder currNum div) (/ div 10))
                #f)]))
  (helper n (expt 10 (- (num-len n) 1))))

(ascending? 1233345)
(ascending? 1243345)

;Задача 5. Да се дефинира функцията (perfect-number? n), която проверява дали числото n e съвършено, т.е. дали е равно на сбора на делителите си.
(define (is-proper-div? num div)
  (cond
    [(= (remainder num div) 0) #t]
    [else #f]))

(define (perfect-number? n)
  (define (check div sum)
    (cond
      [(= div n) (if (= sum n)
                     #t
                     #f)]
      [else (if (is-proper-div? n div)
                (check (+ div 1) (+ sum div))
                (check (+ div 1) sum))]))
  (check 1 0))

(perfect-number? 6)
(perfect-number? 27)
(perfect-number? 28)
(perfect-number? 8128)

;Задача 6. По зададени x и n, да се дефинира функция (calc-sum x n), която пресмята сумата: 1 + x + x^2 + x^3 + ... + x^n. Използвайте не повече от n на брой умножения.
(define (calc-sum x n)
  (define (calc tracker result)
    (cond
      [(> tracker n) result]
      [else  (calc (+ tracker 1) (+ (expt x tracker) result))]))
  (calc 0 0))

(calc-sum 2 0)
(calc-sum 2 1)
(calc-sum 2 2)