#lang racket
; От миналия път:
; Асоциативен списък - списък от точкови двойки (<ключ> . <асоциация>)
(define assocList1 `((a . 6) (b . 5) (c . 4) (d . 3) (f . 2)))
(define assocList2 (list (cons `a 6) (cons `b 5) (cons `c 4) (cons `d 3) (cons `f 2)))
;(equal? assocList1 assocList2) ; -> #t

; Задача 9. Функция (assoc key a-list), която търси key в асоциативния списък a-list и връша първия му елемент с ключ равен на key
(define (assoc key a-list) 
  (cond [(null? a-list)             #f]
        [(equal? key (caar a-list)) (car a-list)] ; caar xs == car (car xs)
        [else                       (assoc key (cdr a-list))]))
; в Racket има вградени функции assq, assv, assoc, които извършват това действие (различават се по смисъла на "равен", т.е. дали сравняват с eq?, eqv? или equal?)
  
; Задача 10. Функция (replace lst dict), която връща като резултат списък, получен от lst, в който елементите са заменени с асоциацията им в dict.
(define (replace lst dict)
  (cond [(null? lst)            `()]
        [(assoc (car lst) dict) (cons (cdr (assoc (car lst) dict)) (replace (cdr lst) dict))]
        [else                   (cons (car lst) (replace (cdr lst) dict))]))

(define (replaceWithMap lst dict)
  (define (replaceElem x)
    (let ([result (assoc x dict)])
      (if result       ; точкова двойка ще се оцени до #t
          (cdr result) ; вторият елемент на двойката е асоциацията
          x)))         ; при #f не е намерена асоциация, т.е. няма с какво да заместваме елемента => оставяме си го съшия
  (map replaceElem lst))

;(replace        `(a b c d e) assocList1) ; -> '(6 5 4 3 e)
;(replaceWithMap `(a b c d e) assocList1) ; -> '(6 5 4 3 e)

; Упражнение 7:
; генериране на списъци от числата в даден интервал
;(range 1 5)     ; -> '(1 2 3 4)
;(range 5)       ; -> '(0 1 2 3 4)
;(range 1 10 3)  ; -> '(1 4 7)
;(range 10 1 -3) ; -> '(10 7 4)

; Задача 0. Да се реализира функция (insertionSort xs), която реализира сортиране чрез вмъкване върху списък.
; първо трябва да се дефинира функция (insert x xs), която добавя елемент в сортиран (възходящо) списък на правилната позиция, така че резултатният списък също е сортиран
(define (insert x xs)
  (cond [(null? xs)     (list x)]
        [(< x (car xs)) (cons x xs)]
        [else           (cons (car xs) (insert x (cdr xs)))]))

;(insert 1 `(2 3 4)) ; -> '(1 2 3 4)
;(insert 3 `(1 2 4)) ; -> '(1 2 3 4)
;(insert 4 `(1 2 3)) ; -> '(1 2 3 4)

(define (insertionSort xs)
  (foldr insert `() xs))

;(insertionSort `(7 3 2 5 1 6 10 8 9 4)) ; -> '(1 2 3 4 5 6 7 8 9 10)

; в Racket има и вградена функция за сортиране - sort
;(sort `(7 3 2 5 1 6 10 8 9 4) <) ; -> '(1 2 3 4 5 6 7 8 9 10)
;(sort `(7 3 2 5 1 6 10 8 9 4) >) ; -> '(10 9 8 7 6 5 4 3 2 1)


; от файла "Задачи за подготовка за първото контролно по ФП"
; Задача 1. Да се дефинира функция (sum-numbers a b), приемаща два аргумента, която намира сумата на числата в интервала [a,b], чиито цифри са в низходящ (>=) ред.
(define (descending-digits? n)
  (define (helper curr lastDigit)
    (cond [(= curr 0)                         #t]
          [(>= (remainder curr 10) lastDigit) (helper (quotient curr 10) (remainder curr 10))]
          [else                               #f]))
  (helper (quotient n 10) (remainder n 10)))

(define (sum-numbers a b)
  (foldr + 0 (filter descending-digits? (range a (+ b 1)))))

;(sum-numbers 1 9)     ; -> 45
;(sum-numbers 199 203) ; -> 200
;(sum-numbers 219 225) ; -> 663

; Задача 2. Да се дефинира функция (num-bigger-elements lst), която за даден списък от числа lst връща като резултат списък с елементи от вида (lsti ni),
; където lsti е i-тият елемент на lst, а ni е броят на елементите на lst, които са по-големи от lsti.
(define (num-bigger-elements xs)
  (define (makePair x)
    (cons x (length (filter (λ (y) (> y x)) xs))))
  (map makePair xs))

;(num-bigger-elements '(5 6 3 4)) ; -> '((5 1) (6 0) (3 3) (4 2))
;(num-bigger-elements '(1 1 1))   ; -> '((1 0) (1 0) (1 0))

; Задача 3. Ако f и g са числови функции и n е естествено число, да се дефинира функция от повисок ред (switchsum f g n), която връща като резултат функция, чиято стойност в дадена
; точка x е равна на f(x)+g(f(x))+f(g(f(x)))+ ... (сумата включва n събираеми).
(define (switchsum f g n)
  (define (helper count prev result)
    (cond [(= count n)  result]
          [(odd? count) (helper (+ count 1) (f prev) (+ result prev))]
          [else         (helper (+ count 1) (g prev) (+ result prev))]))
  (λ (x) (helper 0 (f x) 0)))

;((switchsum (λ (x) (+ x 1)) (λ (x) (* x 2)) 1) 2) ; -> 3
;((switchsum (λ (x) (+ x 1)) (λ (x) (* x 2)) 2) 2) ; -> 9
;((switchsum (λ (x) (+ x 1)) (λ (x) (* x 2)) 3) 2) ; -> 16
;((switchsum (λ (x) (+ x 1)) (λ (x) (* x 2)) 4) 2) ; -> 30

; Задача 4. Да се дефинира функция (repeater str), която получава като аргумент символен низ и връща анонимна функция на два аргумента - count и glue (число и низ).
; Оценката на обръщението към върнатата функция е низ, който се получава чрез count-кратно повтаряне на низа str, при което между всеки две съседни повторения на str стои низът glue.
; Помощна информация. За да съедините няколко низа, може да използвате вградената функцията string-append:
;(string-append "I" "Love" "Racket") ; -> "ILoveRacket"
; Функцията string-append приема произволен брой агрументи и връща низ, който представлява тяхната конкатенация.
(define (repeater str)
  (define (helper count glue)
    (cond [(= count 0) ""]
          [(= count 1) str]
          [else        (string-append str glue (helper (- count 1) glue))]))
  (λ (count glue) (helper count glue)))

;((repeater "I love Racket") 3 " ") ; -> "I love Racket I love Racket I love Racket"
;((repeater "Quack") 5 "!")         ; -> "Quack!Quack!Quack!Quack!Quack"

; Задача 5. Да се дефинира функция (sum-sum-digit a b k), която намира сумата на естествените числа от a до b (0<a≤b), сумата от цифрите на които е кратна на k.
(require math/number-theory)

(define (sum-sum-digit a b k)
  (define (digit-sum n)
    (if (= n 0)
        0
        (+ (remainder n 10) (digit-sum (quotient n 10)))))
  (foldr + 0 (filter (λ (x) (divides? k (digit-sum x))) (range a (+ b 1)))))

;(sum-sum-digit 10 20 3)   ; -> 45
;(sum-sum-digit 101 141 6) ; -> 882

; Задача 6. Да се дефинира функция (max-ordered-sublist lst), която намира най-дългия възходящо сортиран подсписък на списъка от числа lst.
; това е maxOrderedSublist (Зад. 7) от Упражнение 6

; Задача 7. Да се дефинира функция (where list-elements list-predicates), която връща списък от всички елементи на list-elements, за които са изпълнени всички предикати в list-predicates.
(define (where list-elements list-predicates)
  (define (check-element x)
    (andmap (λ (pred?) (pred? x)) list-predicates))
  (filter check-element list-elements))

; (andmap f '(x1 x2 ...)) == (and (f x1) (f x2) ...)
;(andmap (lambda (x) (> x 5)) '(4 7 8)) ; -> #f (не всички са по-големи от 5

;(where '(3 4 5 6 7 8 9 10)     (list even? (λ (x) (> x 5)))) ; -> '(6 8 10) (списък от всички елементи на дадения, които са четни числа, по-големи от 5)
;(where '(3 4 5 7)              (list even? (λ (x) (> x 5)))) ; -> '() (в списъка няма четни числа, по-големи от 5)
;(where '(3 4 5 7 8 9 12 14 15) (list even? (λ (x) (> x 5)))) ; -> '(8 12 14)

; Задача 8. Да се дефинира функция (set-union xs ys), която връща обединението на множествата от числа xs и ys, представени като списъци, наредени във възходящ ред.
; Елементите на резултантното множество също трябва да са наредени във възходящ ред.
(define (set-union xs ys)
  (sort (remove-duplicates (append xs ys)) <))
; така ще правим допълнително триене на повторения и сортиране, но пък става на един ред

; дългото решение, при което избягваме повторения и директно получаваме сортирания списък
(define (set-union-alt xs ys)
  (define (helper xs ys result)
    (cond [(null? xs)            (append result ys)]
          [(null? ys)            (append result xs)]
          [(< (car xs) (car ys)) (helper (cdr xs) ys       (append result (list (car xs))))]
          [(> (car xs) (car ys)) (helper xs       (cdr ys) (append result (list (car ys))))]
          [else                  (helper (cdr xs) (cdr ys) (append result (list (car xs))))]))
  (helper xs ys '()))

;(set-union '(1 3 5 7)           '(5 7 13))              ; -> '(1 3 5 7 13)
;(set-union '(5 7 13)            '(1 3 5 7))             ; -> '(1 3 5 7 13)
;(set-union '(3 5 7 12 13 14 15) '(1 2 3 4 5 7 8 16 17)) ; -> '(1 2 3 4 5 7 8 12 13 14 15 16 17)


; Задача 9. а) Да се реализира функция (sum-digit-divisors n), която намира сумата на тези от положителните цифри на дадено естествено число, които са му делители.
(define (sum-digit-divisors n)
  (let ([digit (remainder n 10)]
        [next (quotient n 10)])
    (cond [(= 0 n)            0]
          [(divides? digit n) (+ digit (sum-digit-divisors next))] ; divides?, при делител 0 просто връща #f, така че може спокойно да пропуснем проверката дали цифра е положителна
          [else               (sum-digit-divisors next)])))

; б) Да се реализира функция (same-sum a b), която намира броя на двойките числа (m, n), за които a <= m < n <= b и функцията sum-digit-divisors връща един и същ резултат.
(define (same-sum a b)
  (define (helper m n)
    (cond [(= n a)                                           0]
          [(= m n)                                           (helper a (- n 1))]
          [(= (sum-digit-divisors m) (sum-digit-divisors n)) (+ 1 (helper (+ m 1) n))]
          [else                                              (helper (+ m 1) n)]))
  (helper a b))

; все едно въртим два вложени for цикъла:
; for(n = b; n > a; --n)
;   for(m = a; m < n; ++m)

;(same-sum 28 35) ; -> 2 (двойките са (28,32) и (29,34))

; Задача 10. "Метрика" наричаме функция, която приема като параметър списък от числа и връща число като резултат. Да се напише функция best-metric?, която приема като параметри
; списък от метрики ms и списък от списъци от числа xss и проверява дали има метрика в ms, която дава по-големи стойности от всички други метрики от ms над всеки от елементите на xss.
(define (smaller-list xs ys)  ; сравнява списъците поелементно, т.е. проверява дали всеки елемент на xs е по-малък от съответния на ys
  (cond [(or (null? xs) (null? ys)) #t]
        [(<= (car xs) (car ys))     (smaller-list (cdr xs) (cdr ys))]
        [else                       #f]))

;(smaller-list `(1 2 3) `(4 5 6)) ; -> #t
;(smaller-list `(1 3 3) `(1 2 3)) ; -> #f

(define (all-smaller-than xss ys)  ; проверява дали всичките елементи на xss са по-малки списъци от ys
  (andmap (λ (xs) (smaller-list xs ys)) xss))

;(all-smaller-than `()                                `(4 5 6)) ; -> #t
;(all-smaller-than `((1 2 3) (2 3 4))                 `(4 5 6)) ; -> #t
;(all-smaller-than `((1 2 3) (2 3 4) (4 5 6))         `(4 5 6)) ; -> #t
;(all-smaller-than `((1 2 3) (2 3 4) (4 5 6) (5 6 7)) `(4 5 6)) ; -> #f

(define (best-metric? ms xss)
  (let ([applied-metrics (map (λ (m) (map m xss)) ms)])    
    (ormap (λ (xs) (all-smaller-than applied-metrics xs)) applied-metrics)))
    
(define (prod xs) (apply * xs))
(define (sum xs) (apply + xs))
;(best-metric? (list sum prod) `((0 1 2) (3 -4 5) (1337 0)))  ; -> #t
;(best-metric? (list car sum) `((100 -100) (29 1) (42)))      ; -> #f

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
