#lang racket

; Задача 1. Да се дефинира функцията (cartesian-product xs ys), която връща като резултат списък от двойки, представящ декартовото произведение на множествата xs и ys, представени чрез списъци.
(define (cartesian-product xs ys)
  (define (helper left right)
    (cond [(empty? left) '()]
          [(empty? right) (helper (cdr left) ys)]
          [else (cons (cons (car left) (car right)) (helper left (cdr right)))]))
  (helper xs ys))

(cartesian-product '(1 2 3 4 5) '(6 7 8)) ; -> '((1 . 6) (1 . 7) (1 . 8) (2 . 6) (2 . 7) (2 . 8) (3 . 6) (3 . 7) (3 . 8) (4 . 6) (4 . 7) (4 . 8) (5 . 6) (5 . 7) (5 . 8))
(cartesian-product '(1 2) '(3 4)) ; -> '((1 . 3) (1 . 4) (2 . 3) (2 . 4))

; втори вариант - с append и map
(define (cartesian-product-map xs ys)
  (apply append
         (map (λ (x) (map (λ (y) (cons x y)) ys))
              xs)))
; За всеки елемент xi от xs конструираме списък от двойки ((xi,y1) (xi,y2) ... (xi,yn)), където y1, y2, ..., yn са елементите на ys.
; Резултатът е списък от списъци (((x1,y1) ... (x1,yn)) ((x2,y1) ... (x2, yn)) ... ((xk,y1) ... (xk,yn))) , поради което се налага apply append, за да получим списък със самите двойки

; Задача 2. Според основната теорема на аритметиката, всяко естествено число, което е по-голямо от 2, може да се представи като произведение на прости числа.
; Да се дефинира функция (factorize n), която приема естествено число n, по-голямо от 1, и връща сортиран списък от елементите на това представяне.
(require math/number-theory)

(define (factorize n)
  (define (helper i)
    (cond [(= n 1)       `()]
          [(divides? i n) (cons i (factorize (quotient n i)))]
          [else           (helper (+ i 1))]))
  (helper 2))

(factorize 2)          ; -> '(2)
(factorize 3)          ; -> '(3)
(factorize 6)          ; -> '(2 3)
(factorize 13)         ; -> '(13)
(factorize 123)        ; -> '(3 41)
(factorize 152)        ; -> '(2 2 2 19)
(factorize 128)        ; -> '(2 2 2 2 2 2 2)
(factorize 1134472500) ; -> '(2 2 3 3 3 5 5 5 5 7 7 7 7 7)
