#lang racket
; Упражнение 3:

; Задача 1. Да се дефинира функция (prime? n), която проверява дали дадено число n е просто.
; a) проверяваме дали някое от числата от 2 до n дели n. Ако да - значи n не е просто.
(define (prime? n)
  (define (helper current)
    (if (> current (/ n 2)) ; най-големият делител на n винаги е <= n/2, така че няма нужда да проверяваме за числата между n/2 и n - те със сигурност не са делители
        #t
        (and (not (= (remainder n current) 0)) (helper (+ current 1)))))
  ;(helper 2))
  ; ако директно извикаме helper, ще получим че 1 е просто, което е грешно. Това става защото helper на първата стъпка ще провери (> 2 (/ 1 2)), което е вярно, и директно ще върне #t
  ; може да избегнем този проблем, като просто в главната функция правим проверка преди да извикаме helper
  (if (<= n 1)
      #f
      (helper 2)))

;(prime? 1) ; -> #f
;(prime? 2) ; -> #t
;(prime? 3) ; -> #t
;(prime? 6)  ; -> #f
;(prime? 11) ; -> #t
;(prime? 61) ; -> #t

; за проверката за делене има вградена функция
(require math/number-theory) ; за divides?
; с require може да включваме библиотеки, подобно на #include в C++

; b) тук тръгваме наобратно - от n/2 надолу, докато стигнем до 2
(define (prime-alt? n)
  (define (helper current)
    (cond [(< current 2)        #t]
          [(divides? current n) #f]
          [else                 (helper (- current 1))]))
  (if (<= n 1)
      #f
      (helper (quotient n 2))))

; Задача 2. Да се дефинира функция (palindrome? n), която проверява дали дадено число n е палиндром.
(define (palindrome? n)
  (define (reverseNumber rest acc)
    (if (= rest 0)
      acc
      (reverseNumber (quotient rest 10) (+ (remainder rest 10) (* acc 10)))))
  (= n (reverseNumber n 0)))

; да проследим изпълнението на reverseNumber:
;    (reverseNumber 123 0) => 123 равно ли е на 0? Не, минаваме към else частта.
; => (reverseNumber 12 (+ 3 (* 0 10)))
; => (reverseNumber 12 3)  => 12 равно ли е на 0?  Не, минаваме към else частта.
; => (reverseNumber 1 (+ 2 (* 3 10)))
; => (reverseNumber 1 32)  => 1 равно ли е на 0?   Не, минаваме към else частта.
; => (reverseNumber 0 (+ 1 (* 32 10)))
; => (reverseNumber 0 321) => 0 равно ли е на 0?   Да, връщаме acc
; -> 321

;(palindrome? 181) ; -> #t
;(palindrome? 5)   ; -> #t
;(palindrome? 18)  ; -> #f

; Задача 3. Да се дефинира функция (countPalindromes a b), която пресмята броя на палиндромите в даден интервал [a, b].
; a) линейно рекурсивно
(define (countPalindromes a b)
  (cond [(> a b)         0]
        [(palindrome? a) (+ 1 (countPalindromes (+ a 1) b))]
        [else            (countPalindromes (+ a 1) b)]))

; b) линейно итеративно
(define (countPalindromesIter a b)
  (define (helper curr end count) ; имаме ли нужда от end?
    (cond [(> curr end)       count]
          [(palindrome? curr) (helper (+ curr 1) end (+ count 1))]
          [else               (helper (+ curr 1) end count)]))
  (helper a b 0))
; Тук end не се променя в никой от случаите, т.е. на всяка стъпка има една и съща стойност - стойността на b.
; Аргументите на главната функция са видими от локалната, така че може вместо end просто да проверяваме директно за b.

; c) като b), но без аргумента end
(define (countPalindromesIter2 a b)
  (define (helper curr count) 
    (cond [(> curr b)         count]
          [(palindrome? curr) (helper (+ curr 1) (+ count 1))]
          [else               (helper (+ curr 1) count)]))
  (helper a 0))

;(countPalindromes 10 20)     ; -> 1
;(countPalindromes 1 10)      ; -> 9
;(countPalindromesIter 10 20) ; -> 1
;(countPalindromesIter 1 10)  ; -> 9

; Задача 4. Да се дефинира функция (sumPalindromes a b), която пресмята сумата на палиндромите в даден интервал [a, b].

; може много лесно да адаптираме решенията на миналата задача
(define (sumPalindromes a b)
  (cond [(> a b)         0]
        [(palindrome? a) (+ a (sumPalindromes (+ a 1) b))] ; само тук има промяна - добавяме самото число вместо единица
        [else            (sumPalindromes (+ a 1) b)]))

;(sumPalindromes 100 150) ; -> 605 (101 + 111 + ... + 141)

(define (sumPalindromesIter a b)
  (define (helper curr sum) ; променили сме count на sum
    (cond [(> curr b)         sum]
          [(palindrome? curr) (helper (+ curr 1) (+ sum curr))] ; както горе - добавяме текущото число вместо единица
          [else               (helper (+ curr 1) sum)]))
  (helper a 0))

;(sumPalindromesIter 100 150) ; -> 605

; Задача 5. Да се дефинира функция (countDivisors n), която пресмята броя на естествените делители на дадено число n.
; Фунцкията да генерира итеративен изчислителен процес.

; отново може лесно да адаптираме решението на countPalindromes, по-точно това от c) подточка
(define (countDivisors n)
  (define (helper curr count)
    (cond [(> curr n)               count]
          [(divides? curr n) (helper (+ curr 1) (+ count 1))] ; променяме единствено проверката
          [else                     (helper (+ curr 1) count)]))
  (helper 1 0)) ; Всяко естествено число се дели на 1 и на себе си

;(countDivisors 1)  ; -> 1 (само 1)
;(countDivisors 2)  ; -> 2 (1, 2)
;(countDivisors 18) ; -> 6 (1, 2, 3, 6, 9, 18)
;(countDivisors 13) ; -> 2 (1, 13)

; Задача 6. Да се дефинира функция (increasingDigits? n), която проверява дали цифрите на числото n са подредени в (строго) нарастващ ред.
; а) като пазим последната цифра
(define (increasingDigits? n)
  (define (helper curr lastDigit)
    (cond [(= curr 0)                        #t] ; за едноцифрено число условието със сигурност е изпълнено
          [(< (remainder curr 10) lastDigit) (helper (quotient curr 10) (remainder curr 10))] ; проверяваме дали предпоследната цифра е < от последната
          [else                              #f]))
  (helper (quotient n 10) (remainder n 10))) ; взимаме числото без последната си цифра, както и самата цифра
; Тук проверяваме за строго неравенство, което означава че при числа с повтарящи се цифри като например 1233 ще считаме че НЕ изпълняват предиката.
; Бихме могли да си ползваме <=, зависи какво точно се има предвид под "нарастващ ред" - тук в условието е специфицирано строго.

;(increasingDigits? 5)    ; -> #t
;(increasingDigits? 15)   ; -> #t
;(increasingDigits? 1234) ; -> #t
;(increasingDigits? 1233) ; -> #f
;(increasingDigits? 1534) ; -> #f
;(increasingDigits? 2134) ; -> #f

; да проследим процеса за 1234:
;    (increasingDigits? 1234)
; => (helper 123 4) => 123 по-малко ли е от 10? Не, минаваме към втория случай - 3 по-малко ли е от 4? Да - влизаме във втората клауза.
; => (helper 12 3)  => 12 по-малко ли е от 10?  Не, минаваме към втория случай - 2 по-малко ли е от 3? Да - влизами във втората клауза.
; => (helper 1 2)   => 1 по-малко ли е от 10? Да, влизаме в първата клауза.
; -> #t

; а за 1534
;    (increasingDigits? 1534)
; => (helper 153 4) => 153 по-малко ли е от 10? Не, минаваме към втория случай - 3 по-малко ли е от 4? Да - влизаме във втората клауза.
; => (helper 15 3)  => 15 по-малко ли е от 10?  Не, минаваме към втория случай - 5 по-малко ли е от 3? Не - минаваме към третия случай.
; => но третият случай е else, в такава клауза винаги се влиза, следователно връщаме стойността на израза от дясно.
; -> #f

; b) вместо cond бихме могли да ползваме or и and
(define (increasingDigits2? n)
  (define (helper curr lastDigit)
    (or (= curr 0)
        (and (< (remainder curr 10) lastDigit)
             (helper (quotient curr 10) (remainder curr 10)))))
  (helper (quotient n 10) (remainder n 10)))

; c) може и без помощна функция - няма да пазим последната цифра, а просто на всяка стъпка ще намираме последната и предпоследната цифра на текущото число.
(define (increasingDigits3? n)
  (or (= n 0)
      (and (< (remainder (quotient n 10) 10) (remainder n 10))
           (increasingDigits3? (quotient n 10)))))

; Задача 7. Да се дефинира функция (calcSum x n), която по зададени x и n изчислява сумата: 1 + x + x^2 + x^3 + ... + x^n.
; a) неефективно - на стъпка i смятаме x^i от нулата
(define (calcSum x n)
  (define (helper counter sum)
    (if (> counter n)
        sum
        (helper (+ counter 1) (+ sum (expt x counter)))))
  (helper 1 1))

;(calcSum 2 0) ; -> 1
;(calcSum 2 1) ; -> 3
;(calcSum 2 2) ; -> 7
;(calcSum 2 3) ; -> 15

; b) може да се възползваме от това, че ако запазим миналото събираемо (x^(i-1)), то текущото може да получим като просто умножим миналото веднъж по x
(define (calcSumOptimal x n)
  (define (helper counter current sum)
    (if (> counter n)
        sum
        (helper (+ counter 1) (* current x) (+ sum current))))
  (helper 1 x 1))

;(calcSumOptimal 2 0) ; -> 1
;(calcSumOptimal 2 1) ; -> 3
;(calcSumOptimal 2 2) ; -> 7
;(calcSumOptimal 2 3) ; -> 15

;(time (calcSum        2 10000))  ; -> cpu time: 79 real time: 77 gc time: 63
;(time (calcSumOptimal 2 10000))  ; -> cpu time: 15 real time: 12 gc time: 15
;(time (calcSum        2 100000)) ; -> cpu time: 1844 real time: 1857 gc time: 783
;(time (calcSumOptimal 2 100000)) ; -> cpu time: 765 real time: 779 gc time: 392