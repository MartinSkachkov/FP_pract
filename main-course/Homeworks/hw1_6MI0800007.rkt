#lang racket

;task 1
(define (divisible-by-k num k)
  (cond
    [(= (remainder num k) 0) #t]
    [else #f]))  

(define (sum-of-digits num)
  (define (result currNum sum)
    (cond
      [(= currNum 0) sum]
      [else (result (quotient currNum 10) (+ sum (remainder currNum 10)))]))
 (result num 0))

(define (count-specials k a b)
  (define (count k a b counter)
    (cond
      [(> a b) counter]
      [else
       (if (and (divisible-by-k a k) (divisible-by-k(sum-of-digits a) k))
           (count k (+ a 1) b (+ counter 1))
           (count k (+ a 1) b counter))]))
  (count k a b 0))            

(count-specials 3 3 9)
(count-specials 5 10 100)
(count-specials 8 100 200)
(count-specials 15 1000 2000)

;task 2
(define (num-len num)
  (define (count num len)
    (cond
      [(= 0 num) len]
      [else (count (quotient num 10) (+ len 1))]))
    (count num 0))

(define (shift-left num realLen)
  (cond
    [(= realLen (num-len num)) (+(*(remainder num (expt 10 (-(num-len num)1)))10)
                                 (quotient num (expt 10 (-(num-len num)1))))]
    [else (* num 10)]))

(define (fix-first-k num k)
 (+(*(quotient num (expt 10 (- (num-len num) k))) (expt 10 (- (num-len num) k)))
   (shift-left(remainder num (expt 10 (- (num-len num) k))) (- (num-len num) k))))

(define (compare currNum ans)
  (if (> currNum ans)
      currNum
      ans))

;56789 -> 67895 -> 68957 -> 68579 -> 68597
(define (max-rot num)
  (define (calculate currNum k ans)
    (cond
      [(= k (- (num-len currNum) 1)) ans]
      [else (calculate (fix-first-k currNum k) (+ k 1) (compare currNum ans))]))
  (calculate num 0 num))

(max-rot 56789)
(max-rot 12490)
(max-rot 38458215)
(max-rot 195881031)
(max-rot 896219342)
(max-rot 69418307) 
(max-rot 257117280)









    