#lang racket
; Точкови двойки
; конструират се чрез cons (конструктор)
;(cons 1 2) ; -> '(1 . 2)
;(cons 5 "string") ; -> '(5 . "string")
; може да достъпваме елементите чрез car и cdr (селектори)
(define a (cons 1 2))
;(car a) ; -> 1
;(cdr a) ; -> 2

; Дроби (рационални числа)
(define (make-rat a b) (cons a b)) ; a/b -> (a . b)
(define (numer x) (car x)) ; numerator
(define (denom x) (cdr x)) ; denominator

(define (+rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (denom x) (numer y)))
            (* (denom x) (denom y))))
(define (-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (denom x) (numer y)))
            (* (denom x) (denom y))))
(define (*rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))
(define (/rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))
(define (=rat x y)
  (= (* (numer x) (denom y))
     (* (denom x) (numer y))))

(define (normalize-rat x)
  (let* ([a (numer x)]
         [b (denom x)]
         [c (gcd a b)])
    (make-rat (/ a c)
              (/ b c))))
(define (+rat-normalized x y)
  (normalize-rat (make-rat (+ (* (numer x) (denom y))
                              (* (denom x) (numer y)))
                           (* (denom x) (denom y)))))

;(+rat (make-rat 1 8) (make-rat 3 8)) ; -> '(32 . 64)
;(+rat-normalized (make-rat 1 8) (make-rat 3 8)) ; -> '(1 . 2)

; Комплексни числа
(define (make-complex a b) (cons a b))
(define (real-part x) (car x))
(define (imag-part x) (cdr x))

(define (+complex x y)
  (make-complex (+ (real-part x) (real-part y))
                (+ (imag-part x) (imag-part y))))
(define (-complex x y)
  (make-complex (- (real-part x) (real-part y))
                (- (imag-part x) (imag-part y))))
(define (*complex x y)
  (make-complex (- (* (real-part x) (real-part y)) (* (imag-part x) (imag-part y)))
                (+ (* (real-part x) (imag-part y)) (* (imag-part x) (real-part y)))))

;(+complex (make-complex 1 5) (make-complex 3 (- 2))) ; -> '(4 . 3)


; Списъци
; Синтаксис - `(1 2 3 4) - в началото има символа "`"
; Може да конструираме списъци и чрез функцията list
;`(1 2 3 4)     ; -> '(1 2 3 4)
;(list 1 2 3 4) ; -> '(1 2 3 4)

; Списъците всъщност се реализират чрез точкови двойки, и съответно се конструират чрез cons.
; Списъкът представлява точкова двойка, съдържаща като първа координата някакъв елемент, а като втора - друг списък. Този списък също е точкова двойка, и т.н.
; На всеки елемент от списъка съответства по една точкова двойка, на някакво ниво на вложение. Последният елемент е представен от точкова двойка, чиято втора координата е празен списък.
; `(1 2 3) == (cons 1 (cons 2 (cons 3 `())))

; Тъй като Racket е динамичен език, списъците могат да бъдат хетерогенни
;`(1 2 3 "string" 4 5.2) ; -> '(1 2 3 "string" 4 5.2)

; Може да имаме и списъци от списъци
;`((1 2 3) (4) (5 6)) ; -> '((1 2 3) (4) (5 6))
; Забележете, че списъците вътре нямат ` пред отварящата скоба


; Задача 0. В Racket има много вградени функции за работа със списъци. Ще демонстрираме някои от тях, като за тези d) нататък ще покажем и примерни реализации.
; a) дали нещо е празен списък
;(null? `())      ; -> #t
;(null? `(1 2 3)) ; -> #f

; b) дали нещо е списък
;(list? `())      ; -> #t
;(list? `(1 2 3)) ; -> #t
;(list? 1)        ; -> #f

; c) глава и опашка на списък
;(car `(1 2))   ; -> 1
;(cdr `(1 2))   ; -> '(2)
;(cdr `(1 2 3)) ; -> '(2 3)
;(car `())      ; car: contract violation

; (cadr xs) е еквивалентно на (car (cdr xs)) - глава на опашката на списъка, което е всъщност втория елемент
;(cadr `(1 2 3 4 5))   ; -> 2
;(caddr `(1 2 3 4 5))  ; -> 3
;(cadddr `(1 2 3 4 5)) ; -> 4
;caddddr обаче няма - само до 4ти елемент може с вградените

;(car `((1 2 3) (4) (5 6)))   ; -> '(1 2 3)
;(cdr `((1 2 3) (4) (5 6)))   ; -> '((4) (5 6))
;(caar `((1 2 3) (4) (5 6)))  ; -> 1
;(caadr `((1 2 3) (4) (5 6))) ; -> 4

; има и first и rest, които са еквивалентни на car и cdr, но работят само за списъци (car/cdr работят и за точкови двойки)
;(first `(1 2 3)) ; -> 1
;(rest `(1 2 3))  ; -> '(2 3)

;(car (cons 1 2))   ; -> 1
;(first (cons 1 2)) ; first: contract violation

; d) дължина на списък
(define (length xs)
  (if (null? xs)
      0
      (+ 1 (length (cdr xs)))))

; Това е една от основните схеми за работа със списъци - разглеждаме два случая:
; - за празен списък, при което приключваме с изпълнението и връщаме някакъв резултат;
; - за непразен списък, при което извършваме някакво действие с главата на списъка (тук може би не се вижда много добре, тъй като никъде не взимаме (car xs),
; но действието което извършваме е преброяване, чрез добавянето на единица) и рекурсивно извикваме функцията върху опашката на списъка.

;(length `())      ; -> 0
;(length `(1 2 3)) ; -> 3

; xs като име на аргумент списък е по-скоро от Haskell, но ще го ползваме и тук. Ако ви е по-удобно, може като име да ползвате lst (за предпочитане не list, тъй като това е име на вградена функция).
; Идеята е, че тъй като в математиката ползваме x като име на произволна променлива, а списъка съдържа няколко елемента,
; то за име на произволен списък взимаме "множествено число" на x, което е xs (все едно много x-ове). Ако имаме втори списък той е ys, третия - zs.

; e) принадлежност към списък
(define (myMember x xs)
  (cond [(null? xs)     #f]
        [(= x (car xs)) #t]
        [else           (myMember x (cdr xs))]))

;(myMember 1 `(1 2 3)) ; -> #t
;(myMember 2 `(1 2 3)) ; -> #t
;(myMember 4 `(1 2 3)) ; -> #f
; Вградената функция member работи по малко по-различен начин - когато елемента принадлежи, не връща просто #t, а списъка от този елемент нататъка
;(member 2 `(1 2 3)) ; -> '(2 3)
; Тъй като Racket е динамичен език, може една функция в различни случаи да връща различни типове.

; Освен това, в Racket, повечето булеви операции приемат всяка стойност различна от #f за #t, така че няма проблем да ползваме резултата на member като булева стойност.
;(if (member 2 `(1 2 3)) "yes" "no") ; -> "yes"
; тук (member 2 `(1 2 3)) се оценява до '(2 3), което се интерпретира като #t

;(myMember `(1 2) `((1 2) (3 4))) ; =: contract violation expected: number?  given: '(1 2)
; = работи само за числа. За да сравняваме по-сложни типове, като списъци, трябва да ползваме предиката equal?
(define (myMemberWithEqual x xs)
  (cond [(null? xs)     #f]
        [(equal? x (car xs)) #t]
        [else           (myMemberWithEqual x (cdr xs))]))

;(myMemberWithEqual `(1 2) `((1 2) (3 4))) ; -> #t
;(myMemberWithEqual `(1 3) `((1 2) (3 4))) ; -> #f
; вграденият member ползва equal?, така че с него може да търсим в списъци от списъци

; f) конкатенация на два списъка
(define (myAppend xs ys)
  (if (null? xs)
      ys
      (cons (car xs) (myAppend (cdr xs) ys))))

;(myAppend `(1 2 3) `(4 5)) ; -> '(1 2 3 4 5)
; Вградения append може да работи и за повече аргументи, подобно на аритметичните операции
;(append `(1 2 3) `(4 5) `(6 7) `(8 9 10)) ; -> '(1 2 3 4 5 6 7 8 9 10)
;(append `(1 2 3) `(4 5 (6 7 (8 9)))) ; -> '(1 2 3 4 5 (6 7 (8 9)))

; g) обръщане на списък
(define (reverse xs)
  (define (helper xs result)
    (if (null? xs)
        result
        (helper (cdr xs) (cons (car xs) result))))
  (helper xs `()))

;(reverse `(1 2 3))     ; -> '(3 2 1)
;(reverse `(1 2 (3 4))) ; -> '((3 4) 2 1)
  
; Задача 1. Да се дефинира функция (minimum xs), която намира най-малкия елемент на списъка xs.
(define (minimum xs)
  (define (helper xs min)
    (cond [(null? xs)       min]
          [(< (car xs) min) (helper (cdr xs) (car xs))]
          [else             (helper (cdr xs) min)]))
  (helper (cdr xs) (car xs))) ; може да се изпълнява само върху непразни списъци (празен списък няма car и cdr части)
                              ; Но дали трябва да обработим този случай? Списък без елементи има ли минимален елемент?
  #|(if (null? xs)
      (error "Empty list")
      (helper (cdr xs) (car xs))))|#

;(minimum `(2 3 1 4 5))   ; -> 1
;(minimum `(2 3 4 5 1 0)) ; -> 0

; Задача 2. Да се дефинира функция (countMaximum xs), която намира броя на срещанията на най-големия елемент на списъка xs.
; първо намираме максимума, после броим елементите равни на него
(define (maximum xs)
  (define (helper xs max)
    (cond [(null? xs)       max]
          [(> (car xs) max) (helper (cdr xs) (car xs))]
          [else             (helper (cdr xs) max)]))
  (helper (cdr xs) (car xs)))

(define (countMaximum xs)
  (define (helper xs max cnt)
    (cond [(null? xs)       cnt]
          [(= max (car xs)) (helper (cdr xs) max (+ cnt 1))]
          [else             (helper (cdr xs) max cnt)]))
  (helper xs (maximum xs) 0))

;(countMaximum `(1 2 3))   ; -> 1
;(countMaximum `(1 3 2))   ; -> 1
;(countMaximum `(1 3 3 2)) ; -> 2
;(countMaximum `(1 3 2 3 2 2 3 3 3 2)) ; -> 5

; но така се налага да обходим списъка два пъти - веднъж за максимума, втори път за броенето
; може да го направим само с едно обхождане
(define (countMaximumAlt xs)
  (define (helper xs max cnt)
    (cond [(null? xs)       cnt]
          [(< max (car xs)) (helper (cdr xs) (car xs) 1)]
          [(= max (car xs)) (helper (cdr xs) max      (+ cnt 1))]
          [else             (helper (cdr xs) max      cnt)]))
  (helper (cdr xs) (car xs) 1))

; Задача 3. Да се дефинира функция (ordered? xs pred), която проверява дали списък е сортиран възходящо/низходящо според подадена функция за сравнение
(define (ordered? xs pred?)
  (cond [(null? xs)                 #t] ; празният списък винаги е сортиран
        [(null? (cdr xs))           #t] ; списък с един елемент (т.е. списък с празна опашка) винаги е сортиран
        [(pred? (car xs) (cadr xs)) (ordered? (cdr xs) pred?)] ; cadr == (car (cdr), т.е. втория елемент
        [else                       #f]))

;(ordered? `(1 2 3 4) <)  ; -> #t
;(ordered? `(1 1 1 1) <)  ; -> #f
;(ordered? `(1 1 1 1) <=) ; -> #t
;(ordered? `(1 2 3 0) <)  ; -> #f
;(ordered? `(1 0 2 3) <)  ; -> #f
;(ordered? `(1 0 2 3) >)  ; -> #f
;(ordered? `(4 3 2 1) >)  ; -> #t

; може да напишем функцията orderedBy от миналия път, ползвайки тази
; първо трябва да конвертираме число до списък
(define (numToListReverse n)
    (if (= n 0)
        `()
        (cons (remainder n 10) (numToListReverse (quotient n 10)))))

(define (numToList n)
    (reverse (numToListReverse n)))

;(numToList 123) ; -> '(1 2 3)

(define (orderedNumber? n pred?)
    (ordered? (numToList n) pred?))

;(orderedNumber? 123 <) ; -> #t
;(orderedNumber? 132 <) ; -> #f

; може да направим и число от списък
(define (listToNum xs)
  (define (helper xs result)
    (if (null? xs)
        result
        (helper (cdr xs) (+ (car xs) (* result 10)))))
  (helper xs 0))

;(listToNum `(1 2 3 4 5)) ; -> 12345