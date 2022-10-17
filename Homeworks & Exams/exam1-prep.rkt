#lang racket
(require math/number-theory)

; Задача 1. Някои числа имат интересни свойства. Например:
; 89    → 8^1 + 9^2                   = 89 * 1
; 695   → 6^2 + 9^3 + 5^4 = 1390      = 695 * 2
; 46288 → 4^3 + 6^4 + 2^5 + 8^6 + 8^7 = 2360688 = 46288 * 51
; Да се дефинира процедура (dig-pow n p), която приема естествено число n (записано с цифри abcd..., които могат да се повтарят) и намира естествено число k - такова, че
; (a^p + b^(p+1) + c^(p+2) + d^(p+3) + ...) = n*k. Ако число k с посоченото свойство не съществува, да се връща -1.
(define (numToList n)
  (define (numToListReverse n)
    (if (= n 0)
        `()
        (cons (remainder n 10) (numToListReverse (quotient n 10)))))
  (reverse (numToListReverse n)))

(define (dig-pow n p)
  (define (makeSum xs i)
    (if (null? xs)
        0
        (+ (expt (car xs) i) (makeSum (cdr xs) (+ i 1)))))
  (let ([result (makeSum (numToList n) p)])
    (if (divides? n result)
        (quotient result n)
        -1)))
  
;(dig-pow 89 1)    ; -> 1  (8^1 + 9^2 = 89 = 89 * 1)
;(dig-pow 92 1)    ; -> -1 (няма k - такова, че 9^1 + 2^2 = 92 * k)
;(dig-pow 695 2)   ; -> 2  (6^2 + 9^3 + 5^4 = 1390 = 695 * 2)
;(dig-pow 46288 3) ; -> 51 (4^3 + 6^4 + 2^5 + 8^6 + 8^7 = 2360688 = 46288 * 51)


; Задача 2.  Да се дефинира процедура (kth-max-min xs), която приема списък от цели числа и връща процедура с параметър естествено число k - такова, че оценката на израза ((kth-max-min xs) k) e
; k-тото по големина отрицателно число в xs. Ако такова не съществува, да се връща грешката “No such number”.
(define (kth-max-min xs)
  (define (kth-element xs k)
    (cond [(null? xs) (error "No such number.")]
          [(= k 1) (car xs)]
          [else    (kth-element (cdr xs) (- k 1))]))
  (let ([negatives-list (sort (remove-duplicates (filter negative? xs)) >)])
    (λ (k) (kth-element negatives-list k))))
    ; може и да позлваме list-ref вместо да си дефинираме наша функция за взимане на k-ти
    #|(λ (k) (if (> k (length negatives-list))
               (error "No such number")
               (list-ref negatives-list (- k 1))))))|#

;((kth-max-min '(1 2 3 4 -5 6 7 -2 -1 0)) 2)  ; -> -2
;((kth-max-min '(-1 0 -1 0 -2 3 1 -1)) 3)     ; No such number
;((kth-max-min '(-4 -3 -5 -8 -7)) 2)          ; -> -4


; Задача 3. Да се дефинира процедура (shuffle xs), получава списък от 2*n елемента във вида ‘(x1 x2 .. xn y1 y2 .. yn) и връща списък във вида ‘(x1 y1 x2  y2 .. xn  yn).
; първи вариант
(define (shuffle xs)
  (define (helper xs ys)
    (if (null? xs)
        `()
        (append (list (car xs) (car ys)) (helper (cdr xs) (cdr ys)))))
        ;(cons (car xs) (cons (car ys) (helper (cdr xs) (cdr ys))))))
  (let ([n (/ (length xs) 2)])
    (helper (take xs n) (drop xs n))))

;(shuffle '(2 5 1 3 4 7))     ; -> '(2 3 5 4 1 7)
;(shuffle '(1 2 3 4 4 3 2 1)) ; -> '(1 4 2 3 3 2 4 1)
;(shuffle '(1 1 2 2))         ; -> '(1 2 1 2)

; втори вариант с ползване на zipWith
(define (zipWith f xs ys)
  (cond [(null? xs) `()]
        [(null? ys) `()]
        [else (cons (f (car xs) (car ys)) (zipWith f (cdr xs) (cdr ys)))]))

(define (shuffleZip xs)
  (let ([n (/ (length xs) 2)])
    (apply append (zipWith list (take xs n) (drop xs n)))))
    ;(apply append (map list (take xs n) (drop xs n))))) ; може и вместо zipWith да ползваме директно map, тъй като в Racket той може да приеме функция на k аргумента, и да я приложи над k списъка


; Задача 4. Да се дефинира предикат (triangular? mat), който получава квадратна числова матрица, представена като списък от списъци, и проверява дали тя е горно триъгълна,
; т.е. дали всичките елементи под главния ѝ диагонал са нули.
(define (triangular? mat)
  (define (helper mat i)
    (cond [(empty? mat)                         #t]
          [(andmap zero? (take (car mat) i)) (helper (cdr mat) (+ i 1))]
          [else                                 #f]))
  (helper (cdr mat) 1))

(triangular? '((1 2 3)
               (0 5 6)
               (0 0 9))) ; -> #t
(triangular? '((0 2 3)
               (0 0 6)
               (1 0 0))) ; -> #f
(triangular? '((1 2 3)
               (1 5 6)
               (0 0 9))) ; -> #f
(triangular? '((1 2 3 4)
               (0 5 6 7)
               (0 0 8 9)
               (0 0 0 9))) ; -> #t
(triangular? '((0 0 1 1)
               (0 1 1 1)
               (0 0 1 1)
               (0 0 0 1))) ; -> #t
