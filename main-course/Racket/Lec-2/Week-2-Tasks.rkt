#lang racket

(define (in-range? x a b)
  (and (<= a x) (<= x b)))

(in-range? 10 1 15)
(in-range? -10 1 15)

;Задача 1. Да се напише функция mygcd a b, която връща НОД(a, b).
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(gcd 7 11)
(gcd 14 21)

;Задача 2. Да се напише функция mymaxdivisor x, която намира най-големия делител d на цялото число x > 1, за който d < x.
(define (mymaxdivisor x)
  (define (helper x currD maxD)
    (cond
      [(>= currD x) maxD]
      [(=(modulo x currD)0) (helper x (+ currD 1) currD)]
      [else (helper x (+ currD 1) maxD)]))
    (helper x 1 1))

(mymaxdivisor 225)
;-------------------------------------------
(define (mymaxdivisoR x)
  (define (helper d)
    (cond
      [(= 0 (remainder x d)) d]
      [else (helper (- d 1))]))
  (helper (- x 1)))

(mymaxdivisoR 225)
(mymaxdivisoR 10)

;Задача 3. Да се дефинира функция sum-odds a b, която намира сумата на нечетните числа в затворения интервал [a, b].
(define (sum-odds a b)
  (define (helper currNum endNum sum)
    (cond
      [(> currNum endNum) sum]
      [(odd? currNum) (helper (+ currNum 1) endNum (+ currNum sum))]
      [else (helper (+ currNum 1) endNum sum)]))
  (helper a b 0))

(define (sum-odd a b)
  (cond
    [(> a b) 0]
    [(odd? a) (+ a (sum-odd (+ 1 a) b))]
    [else (sum-odd (+ 1 a) b)]))

;(sum-odds 1 5)
;(sum-odds 6 8)

;(sum-odd 1 5)
;(sum-odd 6 8)

;Задача 4. Да се дефинира предикат prime? n, който проверява дали естественото число n е просто.
(define (prime? n)
  (define (iterate-to-n currNum n)
    (cond
      [(= currNum n) #t]
      [(= (modulo n currNum) 0) #f]
      [else (iterate-to-n (+ 1 currNum) n)]))
  (cond
    [(= n 1) #t]
    [else (iterate-to-n 2 n)]))

(prime? 1)
(prime? 2)
(prime? 4)
(prime? 7)

;Задача 5. Да се дефинира функция count-palindromes a b, която намира броя на палиндромите в интервала [a, b], където a и b са цели неотрицателни числа и a<b.
(define (reverse-num num)
  (define (helper num res)
    (cond
    [(< num 10)(+ (* res 10) num)]
    [else (helper (quotient num 10) (+ (* res 10 ) (remainder num 10)))]))
  (helper num 0))

;(reverse-num 1234)

(define (is-palindrome? original)
  (= original (reverse-num original)))

;(is-palindrome? 121)

(define (count-palindromes a b)
  (define (counter currNum endNum count)
    (cond
      [(> currNum endNum) count]
      [else (counter (+ 1 currNum) endNum (if (is-palindrome? currNum)
                                              (+ 1 count)
                                              count))]))
  (counter a b 0))

(count-palindromes 0 20)

;Задача 6. Да се дефинира функция count-divisors n, която чрез линейно итеративен процес намира броя на естествените делители на едно естествено число n.
(define (count-divisors n)
  (define (counter n d count)
    (cond
      [(> d n ) count]
      [(= 0 (modulo n d)) (counter n (+ 1 d) (+ 1 count))]
      [else (counter n (+ 1 d) count)]))
  (counter n 1 0))

(count-divisors 14)