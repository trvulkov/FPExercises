#lang racket

; Задача 1. Да се дефинира предикат (automorphic? n), който приема естествено число n и проверява дали n^2 завършва с цифрите на n.
(define (finishes-with a b)
  (cond [(or (= b 0) (= a 0))                  #t]
        [(= (remainder a 10) (remainder b 10)) (finishes-with (quotient a 10) (quotient b 10))]
        [else                                  #f]))

(define (automorphic? n)
  (finishes-with n (sqr n)))

(automorphic? 5) ; -> #t
(automorphic? 25) ; -> #t
(automorphic? 36) ; -> #f
(automorphic? 890625) ; -> #t


(require math/number-theory)
; Задача 2. Ще наричаме едно число кубично просто число, ако е просто и може да бъде представено като разлика на кубовете на две последователни естествени числа. 61 е такова число, защото е просто и 61 = 5^3 - 4^3.
; Дефинирайте процедура (nth-cuban n), която чрез линейно итеративен процес връща n-тото кубично просто число.

; неефективно решение
(define (cuban? n)
  (define (special-diff curr)
    (let ([next (+ curr 1)])
      (- (* next next next) (* curr curr curr))))
  
  (define (check-diff i)
    (cond [(> i n)                #f]
          [(= n (special-diff i)) #t]
          [else                   (check-diff (+ i 1))]))
  
  (and (prime? n) (check-diff 1)))

(define (nth-cuban-ineffective n)
  (define (helper i curr last-found)
    (cond [(> i n)       last-found]
          [(cuban? curr) (helper (+ i 1) (+ curr 1) curr)]
          [else          (helper i       (+ curr 1) last-found)]))
  (helper 1 1 1))


(define (nth-cuban n)
  (define (helper curr i last-found) ; чрез curr просто итерираме през естествените числа, в i отчитаме до кое кубично просто число сме стигнали, а в last-found пазим последното намерено кубично просто
    (let* ([next (+ curr 1)]
           [candidate (- (* next next next) (* curr curr curr))])
      (cond [(> i n)            last-found]
            [(prime? candidate) (helper (+ curr 1) (+ i 1) candidate)]
            [else               (helper (+ curr 1) i       last-found)])))
  (helper 1 1 1))

(nth-cuban 1)    ; -> 7      (8 - 1)
(nth-cuban 2)    ; -> 19     (27 - 8)
(nth-cuban 3)    ; -> 37     (64 - 27)
(nth-cuban 4)    ; -> 61     (125 - 64)
(nth-cuban 5)    ; -> 127    (343 - 216)
(nth-cuban 50)   ; -> 55897  (137^3 - 136^3 = 2571353 - 2515456)
(nth-cuban 100)  ; -> 283669 (308^3 - 307^3 = 29218112 - 28934443

(time (nth-cuban 50)) ; -> cpu time: 157 real time: 156 gc time: 48
(time (nth-cuban-ineffective 50)) ; -> cpu time: 6344 real time: 6402 gc time: 111
