#lang racket

(define (pow x n)
  (cond
    [(= n 0) 1]
    [(< n 0) (/ 1(pow x (- n)))] ; this is not substraction, it's like pattern matching for a negative number (- n)
    [else (* x (pow x (- n 1)))]))

;recursive process(romboid)
(define (fact n)
  (cond
    [(= n 0) 1]
    [else (* n (fact (- n 1)))])) ; отложена операция заради * n

;recursive definition of for loop (linear) опашкова рекурсия
(define (for n i result)
  (cond
    [(<= i n) (for n (+ i 1) (* result i))] ; нямаме отложени операции
    [else result]))

(define (fac n)
  (for n 1 1))

(fac 5)

;---------------------------------------------------------------
;let - (let ([<connect values to the variables' names>]) <tell where they will be used>)
;(let ([<пром1> <изр1>]
;      [<пром2> <изр2>]
;      . . .
;      [<промn> <изрn>])
;   <тяло>)
;Всяка променлива (всяко име) <промi> се свързва с оценката
;на съответния израз <изрi>. След това изразите от <тяло> се
;оценяват последователно в локалната среда, получена чрез
;допълване на текущата среда

;При използване на define областта на действие на
;съответните локални променливи съвпада с цялата среда,
;определена от блока на define. Същевременно, при използване на
;let областта на действие на съответните локални променливи е
;само тялото на let.

(define a 4)

;При оценяването на <изр> интерпретаторът търси извън let израза
;свързванията за всички свободни променливи, които се срещат в <изр>.

(define (test-f)
  (let ([x (+ a 3)])
    (+ a x))) ;searches for a in another environment

(test-f)

(let ([fact (lambda (n)
 (if (= n 0)
     1
     (* n (fact (- n 1)))))]) ;this fact is a matched with a function fact defined outside the let
 (fact 4))

(define (no-args-function)
  (let ([x 5]) ;we link the x with the value 5
    x)) ;in this expression

(no-args-function)

(define (func x y)
   (let ([a (+ 1 (* x y))]
         [b (- 1 y)])
     ;we will use the above definitions in:
     (+ (* x (sqrt a)) (* y b) (* a b))))

(func 3 4)

(define (dist x1 y1 x2 y2)
(let ((dx (- x2 x1))
(dy (- y2 y1)))
(sqrt (+ (expt dx) (expt dy)))))

;Свързването в let се извършва едновременно за всички променливи (имена)
(define (caution)
  (define x 2)
  (let ([x 3] ; x = 3
        [y (+ x 2)]) ; y = 2 + 2 (взима x от define, a не горният)
    (* x y)))

(caution) ; 12, не 15

;let is equivalent to : ((lambda (<пром1> <пром2> ... <промn>) <тяло>) <изр1> <изр2> ... <изрn>)
;---------------------------------------------------------------
;let* 
;(let* ([<пром1> <изр1>]
;       [<пром2> <изр2>]
;       . . .
;       [<промn> <изрn>])
;   <тяло>)

;Оценява се <изр1> и <пром1> се свързва с
;[<изр1>]. Оценява се <изр2> и <пром2> се свързва с [<изр2>]. При
;това, ако в <изр2> се среща <пром1>, при оценяването на <изр2>
;се използва **току-що свързаната** с [<изр1>] оценка на <пром1>(само let го няма това). По
;същия начин се продължава със свързване на следващите
;локални променливи (имена), докато се достигне до последната.
;Тогава се оценява <изрn> и <промn> се свързва с
;[<изрn>]. Ако в <изрn> се срещат някои от имената <промi>, i<n,
;при оценяването им се използват току-що свързаните с тях оценки.
;При така получените свързвания се оценява <тяло>.

;!!!Разликата между let и let* е в начина (реда) на свързване на
;имената и стойностите на локалните променливи. Докато при let
;свързването е едновременно, при let* то е последователно. 

(define (caution2)
  (define x 2)
  (let* ([x 3]
         [y (+ x 2)]) ; при оценката на у се взима горният х от let, а не от define
    (* x y)))

(caution2) ; 15, not 12

;При използване на let* областта на действие на
;локалната променлива <промi> е съвкупността от изрази
;<изрi+1>, … , <изрn> и тялото на let*.

(define (area x1 y1 x2 y2 x3 y3)
(let* ((a (dist x1 y1 x2 y2))
(b (dist x2 y2 x3 y3))
(c (dist x3 y3 x1 y1))
(p (/ (+ a b c) 2)))
(sqrt (* p (- p a) (- p b) (- p c)))))

;---------------------------------------------------------------
;letrec
;Ако искаме да използваме рекурсивна дефиниция в <изр>
;частта на обръщение към специална форма, подобна на let,
;трябва да сме в състояние да преодолеем проблема с
;несвързаните променливи. Това може да стане с помощта на специалната форма letrec. При обръщения
;към тази специална форма може да се извършват локални
;свързвания, при които **рекурсията е допустима**.

(define (factorial n)
  (letrec ([fact-iter (lambda (arg res)
                        (if (= arg 0)
                            res
                            (fact-iter (- arg 1) (* arg res))))])
   (fact-iter n 1)))

(factorial 5)
;((lambda (n 1) (if...)) 1)

;---------------------------------------------------------------
; Lambdas - безименни процедури
;(lambda (<args>) (<body>)) -> procedure (дава ни някаква оценка)
;Т.е обръщаме се към анонимната функция която приема някакви аргументи чрез запазената дума
;lambda и тя се оценява в тялото си до някаква стойност която връща

(lambda (x) (+ x 3)) ;prints procedure

;следните две неща правят едно и също
;(define <name> <expr>)
(define (next arg) (+ arg 4))
(next 5)

(define nexT (lambda (arg) (+ arg 4)))
;(define nexT (lambda (5) (+ 5 4)))
;(define nexT (lambda (5) 9))
;(define nexT 9)

(nexT 5)
;((lambda (arg) (+ arg 4)) 5)
;ако <expr> е ламбда израз (ламбда дефиниция),define определя процедура с име <name>
;иначе define определя променлива с име <name>.

;Оценяване на комбинации, чиито оператори са ламбда изрази.
;В общия случай тези комбинации са от вида:
;((lambda (x1 x2 ... xn) <тяло>) a1 a2 ... an)
;В процеса на оценяване на такава комбинация всички срещания (включвания) на xi в <тяло> се заместват с [ai]
;след което се оценява така полученият израз.

((lambda (x) x) 1)
((lambda (x y) (+ x y)) 3 5)
((lambda (x y) (- x y)) 3 5)

(define (f x y)
  ((lambda (a b) (+ (* x (sqrt a)) (* y b) (* a b))) (+ 1 (* x y)) (- 1 y)))
;((lambda (x1 x2)(<body>))                                 a1          a2)                            
;we calculate what a1 & a2 are then the are passed to the lambda args a & b and then a & b are passed to the body to do the calculations and this procedure returns a result

(f 3 4)

(define (compose-two f1 f2) ;this function returns us a procedure
  (lambda (x) (f2 (f1 x))))

;; On Lambda & Higher Order Functions
(define (moi-aussi-lambda n) (+ 1 n))
;; Those two are the same - fn above, symbol below.
(define je-suis-lambda (lambda (n) (+ 1 n)))

(define increment-by-two (compose-two je-suis-lambda moi-aussi-lambda))
;(define increment-by-two (lambda (x) (je-suis-lambda (moi-aussi-lambda x))))
;(define increment-by-two (lambda (1) (je-suis-lambda (moi-aussi-lambda 1))))
;(define increment-by-two (lambda (1) (je-suis-lambda 2)))
;(define increment-by-two (lambda (1) 3))
;(define increment-by-two 3)

(increment-by-two 1) ;one is the argument passed to the lambda
;((compose-two je-suis-lambda moi-aussi-lambda) 1)
;((lambda (x) (je-suis-lambda (moi-aussi-lambda x)) 1)
;...

;анонимната функция пази указател към средата, в която е оценена 
;---------------------------------------------------------------
;Environments
;Всяка променлива в програма на езика Racket би трябвало да
;се разглежда като означение на определено „място“, в което се
;съхранява свързаната с нея стойност. В така наречения модел на
;средите тези „места“ са елементи на специални структури наречени среди.