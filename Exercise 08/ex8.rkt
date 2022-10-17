#lang racket
; от Упражнение 7:
; Задача 11. "Дълбок списък" ще наричаме списък, който може да съдържа други списъци, а "атом" - елемент на дълбок списък, който не е списък (примерно число или символ).
; "Ниво на влагане" на атом в дълбок списък наричаме броя пъти, който трябва да се приложи операцията car за достигане до атома.
; Да се реализира функция deep-delete, която в даден дълбок списък (в който всеки атом е число) изтрива всички атоми, които са по-малки от нивото им на влагане.
(define (atom? x) (not (pair? x)))

(define (deep-delete xss)
  (define (helper xss depth)
    (define (p? x)
      (and (atom? x) (< x depth))) ; ако има нечислови атоми, ще стане проблем като се опитаме да ги сравним с depth
    (cond [(null? xss)       `()]
          [(list? (car xss)) (cons (helper (car xss) (+ depth 1)) (helper (cdr xss) depth))]
          [(p? (car xss))    (helper (cdr xss) depth)]
          [else              (cons (car xss) (helper (cdr xss) depth))]))
  (helper xss 1))

;(deep-delete `(1 (2 (2 4) 1) 0 (3 (1))))   ; -> (1 (2 (4)) (3 ())))


; Упражнение 8:
; Задача 1. Да се дефинира функция (diagonal mat), която получава квадратна числова матрица mat, представена като списък от списъци, и връща списък от елементите на главния диагонал на матрицата.
(define (diagonal mat)
  (if (null? mat)
      `()
      (cons (car (map car mat)) (diagonal (cdr (map cdr mat)))))) ; (map car mat) взима първия стълб, а (cdr (map cdr mat)) взима матрицата без първи стълб и ред
      ;(cons (caar mat) (diagonal (map cdr (cdr mat))))))

(define matrix `((1 2 3 4)
                 (5 6 7 8)
                 (9 10 11 12)
                 (13 14 15 16)))

;(diagonal matrix) ; -> (1 6 11 16)

; Задача 2. Да се дефинира функция от по-висок ред (tabulate f), която връща като резултат функция. Върнатата функция приема като аргументи целите числа a и b и
; връща като резултат списък от точкови двойки от вида '(x . fx), първите елементи на които са поредните точки от интервала [a, b], а вторите – стойностите на f в тези точки. 
(define (tabulate f)
  (λ (a b)
    (map (λ (x) (cons x (f x)))
         (range a (+ b 1)))))

;((tabulate sqr) 1 5) ; -> '((1 . 1) (2 . 4) (3 . 9) (4 . 16) (5 . 25))

; Задача 3. Да се дефинира функцията (latin-square? xss), която проверява дали матрицата xss е латински квадрат (определение на понятието можете да намерите например на https://en.wikipedia.org/wiki/Latin_square). 
; Можете да приемете, че за символи ще използваме числата от 1 до n, където n е големината (броят на редовете/стълбовете) на матрицата. 
(define (transpose xss) (apply map list xss))

(define (square? xss) ; искаме да проверим дали списък от списъци представя квадратна матрица, тоест дали всички редове са с еднаква дължина, и тя е равна на броя редове
  (apply = (cons (length xss) (map length xss)))) ; (map length xss) ще ни даде списък от дължините на редовете, към него с cons добавяме общия броя редове, и с (apply = ...) проверяваме дали всичките са равни
  ;(andmap (λ (x) (= (length x) (length xss))) xss))
;(square? `((1 2) (3 4)))       ; -> #t
;(square? `((1 2) (3 4 5)))     ; -> #f
;(square? `((1 2) (3 4) (5 6))) ; -> #f

(define (latin-square? xss)
  (define (latin-line? xs)
    (let ([n (length xs)])
      (and (= n (length (remove-duplicates xs))) ; няма повторения (ако има, списъкът без тях ще стане по-къс от първоначалния)
           (andmap (λ (x) (and (<= 1 x) (<= x n))) xs)))) ; всичките елементи на реда са между 1 и n (големината на матрицата, която е равна и на дължината на реда в квадратна матрица) 
  (and (square? xss) ; матрицата наистина е квадратна
       (andmap latin-line? xss) ; във всеки ред няма повторения, и елементите са между 1 и n
       (andmap latin-line? (transpose xss)))) ; във всеки ред на транспонираната матрица (т.е. всеки стълб) няма повторения, и елементите са между 1 и n

(define latin '((1 2 3 4)
                (4 3 2 1)
                (2 4 1 3)
                (3 1 4 2)))

;(latin-square? latin) ; -> #t

; Задача 4. Да се дефинира функция (pair-compose fs), която получава списък (f1 f2 f3 ... fn) от едноаргументни числови функции и връща нова едноаргументна числова функция g - 
; такава, че оценката на (g x) е равна на сумата (f1.f2)(x) + (f3.f4)(x) + ... + (fn-1.fn)(x), където "." означава композиция на функции. Ако оригиналният списък с функции има 
; нечетен брой елементи, то последната функция от списъка се композира с функцията идентитет, която получава един аргумент и го връща без промяна.
(define function-list (list sqr
                            (λ (x) (* x x x))
                            (λ (x) (+ x 1))
                            (λ (x) (- x 1))
                            (λ (x) (+ x 2))))

(define (pair-compose fs)
  (define (composed-list xs)
    (cond [(null? xs)       '()]
          [(null? (cdr xs)) (list (compose (car xs) identity))]
          [else             (cons (compose (car xs) (cadr xs)) (composed-list (cddr xs)))]))
  (λ (x) (foldr + 0 (map (λ (f) (f x)) (composed-list fs)))))

;((pair-compose function-list) 4) ; -> 4106 == (4^3)^2 + (4+1-1) + (4+2).id == 4096 + 4 + 6

; Задача 5. Нека l1 = (a1 a2 ... ak) и l2 = (b1 b2 ... bk) са непразни списъци с еднакъв брой числа.
; Да се дефинира предикат (image? l1 l2), който да връща „истина“ точно когато съществува такова число x, че ai = x + bi за всяко i = 1,..., k.
(define (image? l1 l2)
  (let ([x (- (car l1) (car l2))]) ; x = ai - bi
    (equal? l1
            (map (λ (y) (+ y x)) l2)))) ; получаваме списък с елементи x + bi, и проверяваме дали всеки от тях е равен на съответното ai

;(image? '(1 2 3) '(4 5 6)) ; -> #t
;(image? '(1 2 3) '(1 2 2)) ; -> #f
;(image? '(5 6 7) '(1 2 3)) ; -> #t

; Задача 6. Да се дефинира функция (closest-point xys), която приема списък xys от точки в равнината (представени чрез двойки (x.y)) и връща едноаргументна функция,
; чиято стойност в дадена точка p e най-близката до p точка от списъка xys.
(define (closest-point xys)
  (define (distance p1 p2)
    (let ([x1 (car p1)] [y1 (cdr p1)] [x2 (car p2)] [y2 (cdr p2)])
      (sqrt (+ (sqr (- x1 x2)) (sqr (- y1 y2))))))
  
  ; първи вариант - с argmin ((argmin proc lst) връща елемента, който минимизира резултата на proc) 
  ;(λ (p) (cdr (argmin car (map (λ (q) (cons (distance p q) q)) xys)))))
  ; с map конструираме списък от двойки (distance-to-p . (x.y)), с argmin взимаме тази тройка с минимален първи елемент (тоест минимално разстояние), и с cdr взимаме самата точка

  ; втори вариант - с fold
  (λ (p) (foldl (λ (p1 p2) (if (<= (distance p p1) (distance p p2)) p1 p2)) ; тази ламбда функция връща по-близката до p от две точки
                (car xys)
                (cdr xys))))
  ; Това обхождане с fold ще намери най-близката точка. Ако кръстим ламбда функцията f, то ще се получи нещо такова - (f pn ... (f p3 (f p2 p1))...).
  ; (f p2 p1) ще върне по-близката измежду тях, след това тя ще се сравни с p3 и отново ще се вземе по-близката (тоест вече ще имаме най-близката измежду p1, p2 и p3), и т.н., докато накрая не получим
  ; най-близката измежду p1, p2, ..., pn, тоест най-близката точка измежду всичките в списъка.
  ; Тази техника с fold-ване и някаква функция за сравнение може да се ползва и в други случаи, за намиране на минимален/максимален елемент относно някакво условие.

;((closest-point '((1 . 2) (1.2 . 1.3) (3 . 4) (7 . 10))) '(7.1 . 10))  ; -> '(7 . 10)
;((closest-point '((1 . 2) (1.2 . 1.3) (3 . 4) (7 . 10))) '(1 . 2.1))   ; -> '(1 . 2)
;((closest-point '((1 . 2) (1.2 . 1.3) (3 . 4) (7 . 10))) '(1.2 . 1.3)) ; -> '(1.2 . 1.3)

; Задача 7. В лингвистиката т. нар. разстояние на Левенщайн между две думи a и b се дефинира като минималния брой букви, които трябва да бъдат заменени, вмъкнати или изтрити, за да се преобразува a в b.
; https://en.wikipedia.org/wiki/Levenshtein_distance#Definition
; Да се дефинира функция (naive-levenshtein word1 word2), която намира разстоянието на Левенщайн между две дадени думи, представени като списъци от съставящите ги букви.
(define (naive-levenshtein xs ys)
  (cond [(null? xs)                 (length ys)]
        [(null? ys)                 (length xs)]
        [(equal? (car xs) (car ys)) (naive-levenshtein (cdr xs) (cdr ys))]
        [else                       (+ 1 (min (naive-levenshtein (cdr xs) ys)
                                              (naive-levenshtein xs       (cdr ys))
                                              (naive-levenshtein (cdr xs) (cdr ys))))]))

;(naive-levenshtein `(c a t) `(h a t))               ; -> 1
;(naive-levenshtein `(c a t) `(d o g))               ; -> 3
;(naive-levenshtein `(k i t t e n) `(w r i t t e n)) ; -> 2
;(naive-levenshtein `(k i t t e n) `(s i t t i n g)) ; -> 3

; Задача 8. Ако са дадени два списъка от цели числа xs и ys, да се дефинира функция на (contains-multiple-of-all? xs ys), която проверява дали съществува такъв елемент на xs, който е кратен на всички елементи на ys.
(require math/number-theory)

(define (contains-multiple-of-all? xs ys)
  (define (is-multiple x)
    (andmap (λ (y) (divides? y x)) ys)) ; проверява дали всеки елемент на ys дели x (което е еквивалентно на това x да е кратен на всички елементи на ys)
  (ormap is-multiple xs)) ; проверява дали поне един елемент на xs изпълнява is-multiple, т.е. е кратен на всички елементи на ys

;(contains-multiple-of-all? `(3 4 16) `(2 4 8)) ; -> #t (16 е кратно на 2, 4 и 8)
;(contains-multiple-of-all? `(3 4) `(2 4 8))    ; -> #f (3 не е кратно на нито едно от ys, 4 е кратно на 2 и 4, но не на 8)

; Задача 9. Да се дефинира функция (find-max f a b), която по дадена двуаргументна числова функция f и цели числа a и b намира интервалът (i, j), a ≤ i < j ≤ b,
; за който стойността f(i, f(i+1, ... f(j-1, j))) е най-голяма.
(define (subintervals a b) ; връща списък от всички подинтервали на [a,b] (без тези от само един елемент, тъй като искаме i < j)
  (define (helper i j)
    (cond [(= j a) `()]
          [(= i j) (helper a (- j 1))]
          [else    (cons (cons i j) (helper (+ i 1) j))]))
  (helper a b))

(define (find-max f a b)
  (argmax (λ (interval) (foldr f (cdr interval) (range (car interval) (cdr interval))))
          (subintervals a b))) ; конструираме списък с всеки подинтервал на [a,b], и намираме максималния според функция, която смята стойността в условието, ползвайки fold

  ; ако искаме да намерим самата стойност, може така:
  ;(foldr max 0 (map (λ (lst) (foldr f (cdr lst) (range (car lst) (cdr lst))))
  ;                  (subintervals a b)))) ; конструираме списък с всеки подинтервал на [a,b], чрез map за всеки такъв подинтервал [i,j] получаваме съответната стойност на f, и чрез fold взимаме максималната от тях
  
;(find-max - 1 5) ; -> '(3 . 5)
;(find-max + 1 5) ; -> '(1 . 5)
;(find-max * 1 5) ; -> '(1 . 5)
;(find-max / 1 5) ; -> '(3 . 5)