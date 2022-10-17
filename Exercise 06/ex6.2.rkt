#lang racket
; Упражнение 6
; Задача 1. Да се дефинира функция (insertAtPos n x xs), която добавя елемента x на зададена позиция в списъка xs.
(define (insertAtPosError n x xs)
  (if (= n 0)        ; ако n > length xs, какво ще се случи?
      (cons x xs)
      (cons (car xs) (insertAtPosError (- n 1) x (cdr xs)))))

;(insertAtPosError 3 9 `(0 1 2 3 4)) ; -> '(0 1 2 9 3 4)
;(insertAtPosError 6 9 `(0 1 2 3 4)) ; -> car: contract violation  expected: pair?  given: '()

(define (insertAtPos n x xs)
  (cond [(null? xs) (list x)]                    ; така при n > length xs ще добавим елемента накрая
        ;[(null? xs) (error "No such position")] ; а така ще върнем грешка
        [(= n 0)    (cons x xs)]
        [else       (cons (car xs) (insertAtPos (- n 1) x (cdr xs)))]))

;(insertAtPos 3 9 `(0 1 2 3 4)) ; -> '(0 1 2 9 3 4)
;(insertAtPos 5 9 `(0 1 2 3 4)) ; -> '(0 1 2 3 4 9)
;(insertAtPos 9 9 `(0 1 2 3 4)) ; -> '(0 1 2 3 4 9)

; Задача 2. Да се дефинира функция, която изтрива първото срещане на даден елемент в списък
(define (removeFirst x xs)
  (cond [(null? xs)     `()]
        [(= (car xs) x) (cdr xs)]
        [else           (cons (car xs) (removeFirst x (cdr xs)))]))

;(removeFirst 2 `(1 2 3 2 4 2)) ; -> '(1 3 2 4 2)
;(removeFirst 0 `(1 2 3))       ; -> '(1 2 3)

; Задача 3. Да се дефинира функция, която изтрива всички срещания на даден елемент на списък
(define (removeAll x xs)
  (cond [(null? xs)     `()]
        [(= (car xs) x) (removeAll x (cdr xs))] ; разликата с горната функция е само тук - вместо да спрем обработването и да върнем резултат, просто продължаваме нататък
        [else           (cons (car xs) (removeAll x (cdr xs)))]))

;(removeAll 2 `(1 2 3 2 4 2)) ; -> '(1 3 4)

; Задача 4. Функция (removeDuplicates xs), която премахва всички повторни срещания на елементи от списъка
; a)
(define (removeDuplicates xs)
  (define (helper found rest) ; във found ще натрупваме първите срещания на елементите
    (cond [(null? rest)              (reverse found)] ; първите срещания ще се натрупат наобратно, тъй като всяко следващо добавяме в началото на списъка
          [(member (car rest) found) (helper found (cdr rest))]
          [else                      (helper (cons (car rest) found) (cdr rest))]))
  (helper `() xs))

; b)
(define (removeDuplicates2 xs)
  (if (null? xs)
      `()
      (cons (car xs) (removeDuplicates2 (removeAll (car xs) (cdr xs))))))

;(removeDuplicates  `(1 2 1 1 2 2 2 3 4 3 3 3 4 4)) ; -> '(1 2 3 4)
;(removeDuplicates2 `(1 2 1 1 2 2 2 3 4 3 3 3 4 4)) ; -> '(1 2 3 4)

; Задача 5. Функция (sublistBetween start end xs), която взима подсписъка на xs между позициите start и end
(define (sublistBetween start end xs)
  (take (drop xs start) (- end start)))

;(sublistBetween 2 3 `(0 1 2 3 4 5 6 7 8))  ; -> '(2)
;(sublistBetween 2 6 `(0 1 2 3 4 5 6 7 8))  ; -> '(2 3 4 5)
;(sublistBetween 0 9 `(0 1 2 3 4 5 6 7 8)) ; -> '(0 1 2 3 4 5 6 7 8)

; Задача 6. Функция (countOcccurrences subxs xs), която връща броя срещания на subxs в xs.

; Първо ще дефинираме функция, проверяваща дали един списък е префикс на друг списък,
; т.е. дали вторият списък започва с първия
; а) с обхождане на списъците
(define (prefix? subxs xs)
  (cond [(null? subxs)                 #t] ; празният списък е префикс на всеки списък
        [(null? xs)                    #f] ; празният списък няма префикси    
        [(equal? (car subxs) (car xs)) (prefix? (cdr subxs) (cdr xs))]
        [else                          #f]))

;(prefix? `(1 2 3) `(1 2 3 4))         ; -> #t
;(prefix? `(1 2 3) `(0 1 2 3 4))       ; -> #f
;(prefix? `(1 2 3) (cdr `(0 1 2 3 4))) ; -> (prefix? `(1 2 3) `(1 2 3 4)) -> #t
;(prefix? `(1 2 3) `(1 2 3))           ; -> #t

; b) с използване на take
(define (prefixWithTake? subxs xs)
  (equal? subxs (take xs (length subxs))))

; вече самата функция за броене на срещанията
(define (countOcccurrences subxs xs)
  (define (helper curr cnt)
    (cond [(null? curr)         cnt]
          [(prefix? subxs curr) (helper (cdr curr) (+ cnt 1))]
          [else                 (helper (cdr curr) cnt)]))
  (helper xs 0))

;(countOcccurrences `(1) `(1 1 1 1 1))                    ; -> 5
;(countOcccurrences `(1 2 3) `(1 2 3 4 2 3 1 2 3 4 1 2))  ; -> 2
;(countOcccurrences `(1 2 3) `(4 5 6 7))                  ; -> 0
;(countOcccurrences `(1 2 3) `(2 3 4 5 6 7))              ; -> 0

; Задача 7. Функция (maxOrderedSublist xs), която връща най-дългия възходящо сортиран подсписък от списъка xs.

(define (orderedPrefix xs) ; ще намира сортираната част в началото на списъка (ако въобще има такава)
  (cond [(null? xs)              `()]
        [(null? (cdr xs))        xs]
        [(<= (car xs) (cadr xs)) (cons (car xs) (orderedPrefix (cdr xs)))]
        [else                    (list (car xs))]))

;(orderedPrefix `(5 6 7 8 1 2 3 4 5)) ; -> '(5 6 7 8)
;(orderedPrefix `(4 3 2 1 5))         ; -> '(4)

(define (maxOrderedSublist xs)
  (define (helper max curr)
    (define currPrefix (orderedPrefix curr)) ; това може да стане и с let
    (cond [(null? curr)                         max]
          [(< (length max) (length currPrefix)) (helper currPrefix (cdr curr))]
          [else                                 (helper max (cdr curr))]))
  (helper `() xs))

;(maxOrderedSublist '(1 2 2 2 2 1 2 3 4 5 6 1 2))   ; '(1 2 3 4 5 6)
;(maxOrderedSublist '(1 2 2 2 2 2 1 2 3 4 5 6 1 2)) ; '(1 2 2 2 2 2)
;(maxOrderedSublist `(4 3 2 1))                     ; '(4)

; Задача 8. Функция (flatten xss), която приема списък от списъци
; (които също могат да са от списъци, т.е. имаме произволно ниво на вложение)
; и връша списък само от елементи, т.е. списък без вложени списъци
; например `((1 2 3) (4 5 6) ((7 8) (9 10 (11 (12)))))) трябва да стане `(1 2 3 4 5 6 7 8 9 10 11 12)

(define (concat xss) ; конкатенира всички елементи на списък от списъци
  (foldr append `() xss))
(define (concat2 xss)
  (apply append xss))

(define (flatten xss)
  (cond [(null? xss) `()]
        [(list? xss) (concat (map flatten xss))]
        [else        (cons xss `())]))

;(flatten `((1 2 3) (4 5 6) ((7 8) (9 10 (11 (12))))))              ; -> '(1 2 3 4 5 6 7 8 9 10 11 12)
;(flatten `((1 (2 (3 (4 (5 6 (7 8 (9)))))) 10 (11 12 (13 14) 15)))) ; -> '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)
