#lang racket

;variables-------------------------------------------
;(define <var> <val>)
(define plus +) ;#<procedure:+>
(define a 5)
(define y (+ 5 3))
(define hh (+ y 3))
;(define z (+ y z)) ;err

(quote quote) ;'quote
(quote (+ 2 3)) ;'(+ 2 3)

(zero? a) ;#f
(zero? 0) ;#t
(positive? a) ;#t
(number? a) ;#t
(char? a) ;#f
(symbol? symbol?) ;#f
(symbol? (quote symbol?)) ;#t
(symbol? "str") ;#f
(procedure? procedure?) ;since procedure? is evaluated to procedure then procedure? procedure returns #t

;functions--------------------------------------------
;(define(<name> <args>) (<body>))
(define (h x) (+ hh 3))
(define (sum x y) (+ x y))
(define (square x) (* x x)) ;(square 5) → 25
(define (1+ k) (+ k 1)) ;(square (1+ (square 3))) → 100
(define (f x y) ( + (square(1+ x)) (square y) 5))

(define (g x) (- (g (+ x 1)) 1)) ;endless recursion
(define (h1) (+ 5 3)) ;no args function

;condition operators----------------------------------
;(if <cond> <do #t> <do #f>)
(if + 3 5) ;everything except for #f is true in the language including procedures and 0 (+ is evaluated to #t so we execute 3)
(if 0 5 7) ;5
(if (positive? -3)
    "Positive"
    "Not Positive")
(if (< 3 5)
    (+ 7 3)  ;#t case
    (- 4 2) );#f case

(define (abs x)
  (if (< x 0)
      (- x)
      x))
(abs -5)
(abs (- 3 5))

(define (func x y)
  (if (< x y)
      (+ x y)
      (* x y)))
(func 2 3)
(func 3 2)

(define (proc x y)
  ((if (< x y)
      +
      *) x y)) ;if statement will return a procedure a.k.a function and then will be applied to the x and y (<procedure> x y)
(proc 2 3)

;-----------------------------------------------------
;(cond
; [(<cond1>) (<operations>)]
; [(<cond2>) (<operations>)]
; ...
; [else (<operations>)])

(cond
   [(positive? -5) (error "doesn't get here")]
   [(zero? -5) (error "doesn't get here, either")]
   [(positive? 5) 'here])

(define (grade x)
  (cond
    [(>= x 5.5) "Otlichen"]
    [(>= x 4.5) "Mn.Dobyr"]
    [(>= x 3.5) "Dobyr"]
    [(>= x 2.5) "Sreden"]
    [else "slab"]))
(grade 3.7)

;Logical operations-----------------------------------
(not #t)
(and (< 2 3) (<=(+ 2 6) 8)) 

(define (devisible? a b)
  (= (remainder a b) 0))

(define (leap? y)
  (and (divisible? y 4) (or (not (divisible? y 100)) (divisible? y 400))))