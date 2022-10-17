import Data.Char(ord, isDigit, isUpper, toUpper)
import Data.List(group)

{- Списъци в Haskell - работят по подобен начин като тези в Racket, но синтаксиса е малко по-различен, и благодарение на pattern matching-а на практика ще работим с тях по малко по-различен начин
    []   - празен списък
    x:xs - операторът ':' конструира списък с глава x и опашка xs, като (cons x xs) в Racket
    head (x:xs) - връща главата x, подобно на car/first в Racket
    tail (x:xs) - връща опашката xs, подобно на cdr/rest в Racket

    [1,2,3] - списък с елементи 1, 2 и 3, еквивалентно на 1:(2:(3:[]))

В Haskell обаче списъците са хомогенни, за разлика от тези в Racket (които поради динамичното типизиране на езика можеха да съдържат различни типове)
    [а]     - списък от елементи от типа а
    [[a]]   - списък от списъци с елементи от типа а
    [[[a]]] - списък от списъци от списъци с елементи от типа а
    [[[[a]]]] и т.н. -}

-- Да напишем функция sum, която сумира елементите на списък:
-- 1во - както бихме го написали на Racket:
sum' :: [Int] -> Int
sum' xs = if null xs then 0 else head xs + sum' (tail xs)
-- Но в Haskell имаме pattern matching:
sum'' :: [Int] -> Int
sum'' []     = 0
sum'' (x:xs) = x + sum'' xs
-- pattern matching-а ни позволява все едно да "декомпозираме" стойността до нейните съставни - в случая на списък, до главата и опашката му. Така може да ги достъпим директно,
-- без да се налага да извикваме head и tail. Като цяло при работа със списъци ще използваме този подход, може да забравите за постоянното писане на car и cdr от Racket.
-- Тук много рядко ще се налага експлицитно да викаме head и tail (може например при някакви map-ове да се наложи).

-- Задача 0. Примерни реализации на някои функции върху списъци.
-- пишем имената с ' накрая, тъй като функциите вече съществуват и стават конфликти при повторно ползване на имената им

-- проверка дали списък е празен
null' :: [a] -> Bool
null' [] = True
null' _  = False
-- 'a' е произволен тип - може да бъде Int, Float, Char, и т.н.
-- Такива произволни типове се пишат с малки букви, потенциално може да имат и по-детайлно име, но като конвенция се пишат с a, b, и т.н.
-- Ще ги ползваме често при списъци, тъй като много от функциите няма да зависят от конкретния тип на елементите на списъка.

-- взимане на глава на списък
head' :: [a] -> a
head' (x:_) = x -- тъй като опашката тук не ни интересува, я заменяме с '_'

-- взимане на опашка на списък
tail' :: [a] -> [a]
tail' (_:xs) = xs -- тъй като главата тук не ни интересува, я заменяме с '_'

-- дължина на списък
length' :: [a] -> Integer
length' []     = 0
length' (_:xs) = 1 + length' xs

-- проверка дали число е елемент на списък от числа
elem' :: Int -> [Int] -> Bool
elem' _ []     = False
elem' y (x:xs) = x == y || elem' y xs
-- Вграденият elem работи и за други типове освен цели числа, но декларацията му не е просто elem :: a -> [a] -> Bool, тъй като не всеки тип 'a' ще има дефиниран оператор (==). По-нататък ще разгледаме този детайл.

-- функции от по-висок ред
map' :: (a -> b) -> [a] -> [b]
map' _ []     = []
map' f (x:xs) = f x : map' f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ []     = []
filter' p (x:xs) = if p x then x : filter' p xs else filter' p xs

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' _ v []     = v
foldr' f v (x:xs) = f x (foldr' f v xs)

-- reverse ползвайки fold
reverse' :: [a] -> [a]
reverse' lst = foldr (\x xs -> xs ++ [x]) [] lst  


-- Задача 1. Да се дефинира функция isAscending :: Integer -> Bool, която проверява дали цифрите на число са във възходящ ред. Функцията да получава число, но да работи със списък от цифрите му.
isAscending :: Integer -> Bool
isAscending num = isSorted $ digits num
    where
        digits x = if x < 10 then [x] else digits (x `div` 10) ++ [x `mod` 10]
  
        isSorted []         = True
        isSorted (x:[])     = True -- друг вариант за pattern за едноелементен списък е [x]
        isSorted (x1:x2:xs) = x1 <= x2 && isSorted (x2:xs) -- с pattern patching-а е много по-лесно да вземем първите няколко елемента - иначе щяхме да имаме head, head на tail, и т.н. 
        -- Когато ползваме образец като последния, в който предполагаме че списъкът има поне 2 елемента, най-вероятно ще се наложи първо да сме разгледали случая, 
        -- когато списъкът има само 1 елемент, за да се подсигурим че наистина има втори елемент който да вземем.

-- Задача 2. Нека as = [a1, a2 … , ak] и bs = [b1, b2 … , bk] са непразни списъци с еднакъв брой числа. Да се дефинира предикат isImage :: [Int] -> [Int] -> Bool, който да връща „истина“ точно 
-- когато съществува такова число x, че ai = x + bi за всяко i = 1,..., k.

-- 1ви вариант - в стил подобно на Racket
isImage :: [Int] -> [Int] -> Bool
isImage as bs = as == map (+x) bs
    where x = head as - head bs

-- 2ри вариант - ползвайки pattern matching
isImage' :: [Int] -> [Int] -> Bool
isImage' (a:as) (b:bs) = (a:as) == map (+ (a - b)) (b:bs)

-- 3ти вариант - ползвайки синоними за да избегнем повторенията
isImage'' :: [Int] -> [Int] -> Bool
isImage'' as@(a:_) bs@(b:_) = as == map (+ (a - b)) bs
-- @ е начин за създаване на синоними, т.е. тук as и (а:_) са две имена на една и съша стойност - списък с първи елемент а и произволна опашка.
-- Както споменах по-горе, чрез pattern matching-а все едно декомпозираме стойността, но понякога може да искаме хем да се обръщаме към съставните ѝ части, хем към цялата стойност,
-- например в тази задача искаме да може да вземем главите на списъците за да сметнем x, но искаме и самите списъци, за да приложим map и да сравним за равенство.
-- В такива случаи са удобни синонимите - чрез тях може да имаме както променлива, с която да взимаме цялата стойност, така и променливи за всяка нейна съставна част.

-- 4ти вариант - с all и zip
isImage''' :: [Int] -> [Int] -> Bool
isImage''' (a1:as) (b1:bs) = all (\ (ai, bi) -> ai == x + bi) (zip as bs)
    where x = a1 - b1
 
-- Задача 3. Да се дефинира функция pack :: [Int] -> [[Int]], която получава списък от цели числа, и връща списък от списъци, в който поредиците от последователни елементи са "опаковани" в отделни списъци.
pack :: [Int] -> [[Int]]
pack []     = []
pack (x:xs) = (x : takeWhile (==x) xs) : pack (dropWhile (==x) xs)
-- takeWhile е функция, която освен списък получава и предикат, и взима елементи от списъка докато не стигне до такъв който не изпълнява предиката
-- dropWhile пък маха елементи докато не стигне до такъв

{- List Comprehension - конструиране на списък чрез определяне на неговия обхват, подобно на range в Racket, но не е отделна функция
   [a..b]    - конструира списък [a, a+1, a+2,  ..., b]
   [a,h,..b] - конструира списък [a, a+h, a+2h, ..., b]

   [x | x <- xs, predicate] - конструира списък от тези елементи на xs, за които predicate e верен, подобно на filter
   [x | x <- xs, predicate1, predicate2, ..., predicateN] - конструира списък от тези елементи на xs, за които са верни всичките предикати. Доста по-удобно от N вложени filter-а.

   [f x | x <- xs] - ако f е функция на един аргумент ще получим списък, в който към всеки елемент е приложена f (подобно на map)

   може да комбинираме двете горни функционалности:
   [f x | x <- xs, predicate x] - като map върху filter-нат списък

   [(x,y) | x <- xs, y <- ys] - ще конструира списък от двойки с първи елемент от xs и втори от ys
                              - взимаме 1ви елемент от xs -> изчерпваме ys -> взимаме 2ри от xs -> изчерпваме ys -> ...
                              - всъщност ще се получи нещо като декартово произведение, ако мислим за списъците като множества -}

-- Задачи за list comprehension                            
-- Задача 4. Да се дефинира функция divisors :: Int -> [Int], която генерира списък от всички (собствени) делители на дадено число
divisors :: Int -> [Int]
divisors n = [k | k <- [1 .. n `div` 2], n `mod` k == 0]

-- Задача 5. Да се дефинира функция prodSumDiv :: Int -> Int -> Int -> Int, която получава цели числа a, b и k и намира произведението на числата в [a,b], сумата от делителите на които е кратна на k.
prodSumDiv :: Int -> Int -> Int -> Int
prodSumDiv a b k = product [x | x <- [a..b], sum (divisors x) `mod` k == 0]

-- Задача 6. Да се дефинира функция removeNb :: Int -> [(Int, Int)], която приема естествено число n и връща списък от двойки естествени числа (a, b) – такива, че:
-- 1. a и b са по-малки от n,
-- 2. тяхното произведение е равно на сумата от числата от 1 до n без a и b.
removeNb :: Int -> [(Int, Int)]
removeNb n = [(a, b) | a <- [1..n],
                       b <- [1..n],
                       a * b == sum [x | x <- [1..n], x /= a, x /= b]]


-- Низовете в Haskell са просто списъци от Char-ове (type String = [Char]).
-- С тях се работи по същия начин както се работи със списъци: можем да ги обхождаме с рекурсия, да ги map-ваме, да ги генерираме с list comprehension и т.н.

-- Задача 7. Да се дефинира функция digits :: String -> String, която получава низ и намира цифрите в него.
digits :: String -> String
digits str = [ch | ch <- str, isDigit ch] -- isDigit е вградена функция от библиотеката Data.Char
 
-- Задача 8. Да се дефинира функция digitsSum :: String -> Int, която намира сумата на цифрите в даден низ.
digitsSum :: String -> Int
digitsSum "" = 0 -- "" тук е празен низ, което е еквивалентно на празен списък. Може да ползваме и [], но с "" става по-ясно че работим конкретно със низове
digitsSum (c:cs)
    | isDigit c = ord c - ord '0' + digitsSum cs -- ord връща ASCII кода на цифрата, но за да получим самата цифра изваждаме ASCII кода на 0 (например за 5: ord '5' - ord '0' = 53 - 48 = 5)
    | otherwise = digitsSum cs

-- 2ри вариант - ползваме миналата задача
digitsSum' :: String -> Int
digitsSum' cs = sum [ord c - ord '0' | c <- digits cs]

-- Задача 9. Да се дефинира функция capitalize :: String -> String, която прави всички малки букви в даден String главни.
capitalize :: String -> String
capitalize cs = map toUpper cs -- toUpper е вградена функция от библиотеката Data.Char, която конвертира малка буква към главна (и не променя главни букви)

-- Задача 10. Да се дефинира функция isCapitalized :: String -> Bool, която проверява дали всички букви в даден String са главни.
isCapitalized :: String -> Bool
isCapitalized ""     = True
isCapitalized (c:cs) = isUpper c && isCapitalized cs

-- 2ри вариант - с all
isCapitalized' :: String -> Bool
isCapitalized' str = all isUpper str

-- Задача 11. Да се дефинира функция nOrMoreVowels :: [String] -> Int -> [String], която получава списък от думи и число n и връща само думите с поне n гласни.
isVowel :: Char -> Bool
isVowel c = elem c "aeiouy"

countVowels :: String -> Int
--countVowels word = length $ filter isVowel word    -- 1ви вариант
countVowels word = length [c | c <- word, isVowel c] -- 2ри вариант

nOrMoreVowels :: [String] -> Int -> [String]
--nOrMoreVowels words n = filter (\word -> countVowels word >= n) words -- 1ви вариант
nOrMoreVowels words n = [word | word <- words, countVowels word >= n]   -- 2ри вариант

-- Задача 12. Да се дефинира функция isPrefixOf :: String -> String -> Bool, която проверява дали първия ѝ аргумент е префикс на втория.
isPrefixOf' :: String -> String -> Bool
isPrefixOf' [] _          = True  -- празният низ е префикс на всичко
isPrefixOf' _  []         = False -- няма как непразен низ да е префикс на празен низ.
isPrefixOf' (x:xs) (y:ys) = (x == y) && isPrefixOf' xs ys

-- Задача 13. Да се дефинира функция isInfixOf :: String -> String -> Bool, която проверява дали първия ѝ аргумент е инфикс на втория.
-- 1ви вариант
isInfixOf' :: String -> String -> Bool
isInfixOf' [] _  = True
isInfixOf' _  [] = False
isInfixOf' xs ys = isPrefixOf' xs ys || isInfixOf' xs (tail ys)

-- 2ри вариант
-- първо - функция, която генерира всички опашки на даден списък
tails' :: [a] -> [[a]]
tails' []        = [[]] -- ако върнем директно [], то празният списък няма да участва като опашка
tails' ys@(_:xs) = ys : tails' xs
-- второ - проверяваме дали първият низ е префикс на някоя от опашките на втория низ. 
-- Това дефакто правим и в първия вариант, но там обхождаме опашките една по една, а тук ползваме вградената функция any
isInfixOf'' :: String -> String -> Bool
isInfixOf'' xs ys = any (isPrefixOf' xs) (tails' ys)
-- isPrefixOf, isInfixOf и tails всъщност са вградени функции от библиотеката Data.List. Библиотечната реализация на isInfixOf е подобна на втория вариант тук.

-- Задача 14. Да се дефинира функция longestSubstring :: String -> Int, която намира дължината на най-дългия подниз в даден низ, състоящ се от еднакви символи.
longestSubstring :: String -> Int
longestSubstring str = maximum [length substring | substring <- group str]
-- group е вградена функция (от Data.List), която разделя списък на списъци, всеки от които има само еднакви елементи. Например group "Mississippi" -> ["M","i","ss","i","ss","i","pp","i"].
-- Действието е подобно на pack който дефинирахме по-горе, но не е ограничено до списъци от цели числа, и имплементацията е по-различна.

-- Задача 15. Да се дефинира функция tighten, която "сгъстява" низ от думи, като премахва интервалите в началото и в края на низа, а между всеки две думи оставя по един интервал. 
-- Ако низът не съдържа думи, резултатът е празен низ.
tighten :: String -> String
tighten ""             = ""
tighten (' ':cs)       = tighten cs -- премахваме интервали в началото
tighten (c:' ':' ':cs) = tighten (c:' ':cs) -- премахваме повторение на интервали между думи, докато не остане само един
tighten (c:' ':c':cs)  = c : ' ' : tighten (c':cs) -- Тъй като вече сме преминали през горния ред и сме продължили надолу, можем да сме сигурни, че c' не е интервал, т.е. е буква. 
                                                   -- Значи продължаваме "сгъстяването" в остатъка от низа
tighten (c:cs)         = c : tighten cs -- Тъй като сме минали през всичките горни редове и сме стигнали до тук, можем да сме сигурни, че сме "вътре" в дума, т.е. c е буква и след него следва още една буква.
                                        -- Значи взимаме първата буква и обработваме остатъка.

-- 2ри вариант
tighten' :: String -> String
tighten' = unwords . words



main :: IO()
main = do
        -- Конструиране на списъци
    -- print [1,2,3]        -- -> [1,2,3]
    -- print $ 1:[]         -- -> [1]
    -- print $ 1:[2,3]      -- -> [1,2,3]
    -- print $ 1:(2:(3:[])) -- -> [1,2,3]
          -- конкатенацията на списъци (append в Racket) тук е инфиксен оператор: (++)
    -- print $ [1,2,3] ++ [4,5,6] -- -> [1,2,3,4,5,6]
  
    -- разгледайте файла "Някои функции за работа със списъци"
    
    -- print $ map (\x -> x + 3) [1,2,3] -- -> [4,5,6]
    -- -- вместо ламбда може да ползваме операторно сечение
    -- print $ map (+3) [1,2,3]          -- -> [4,5,6]

    -- print $ filter odd [1,2,3,4]  -- -> [1,3]
    -- print $ filter (<3) [1,2,3,4] -- -> [1,2]

    -- print $ foldr (+) 0 [1,2,3]     -- -> 6

    -- print $ all odd [1,3,5]  -- -> True
    -- print $ any even [1,2,3] -- -> True

        -- Задачи
    -- print $ isAscending 234 -- -> True
    -- print $ isAscending 243 -- -> False
    
    -- print $ isImage [1,2,3] [4,5,6] -- -> True
    -- print $ isImage [1,2,3] [4,5,7] -- -> False
      
    -- print $ takeWhile even [2,4,6,1,3,5] -- -> [2,4,6] (спираме при първия нечетен елемент)
    -- print $ dropWhile even [2,4,6,1,3,5] -- -> [1,3,5]
    -- print $ pack [1,1,1,1,2,3,3,1,1,4,5,5,5,5] -- -> [[1,1,1,1],[2],[3,3],[1,1],[4],[5,5,5,5]]
    
        -- List comprehension
    -- print [1..5]    -- -> [1,2,3,4,5]
    -- print [1,3..10] -- -> [1,3,5,7,9]
    -- print [9,7..1]  -- -> [9,7,5,3,1]

    -- print [x | x <- [1..10], even x]           -- -> [2,4,6,8,10]
    -- print [x | x <- [1..5], True]              -- -> [1,2,3,4,5]
    -- print [x | x <- [1..5], False]             -- -> []
    -- print [x | x <- [1..20], x < 10, x*x > 10] -- -> [4,5,6,7,8,9]
    -- print [x*x | x <- [1..5]]                  -- -> [1,4,9,16,25]
    -- print [x*x | x <- [1..5], odd x]           -- -> [1,9,25]
    -- print $ product [x*x | x <- [1..5], odd x] -- -> 225
    -- print [(x,y) | x <- [1,2,3], y <- ['a','b']] -- -> [(1,'a'),(1,'b'),(2,'a'),(2,'b'),(3,'a'),(3,'b')]
    
        -- Задачи за List comprehension
    -- print $ divisors 245         -- -> [1,5,7,35,49]
    -- print $ prodSumDiv 1 10 3 -- -> 24 (1*4*6)

    -- print $ removeNb 10  -- -> [(6,7),(7,6)]
    -- print $ removeNb 11  -- -> []
    -- print $ removeNb 17  -- -> [(10,13),(13,10)]
    -- print $ removeNb 20  -- -> [(14,14)]

        -- Задачи за низове
    -- print $ digits "d321as231dSdadaSA" -- -> "321231"
    -- print $ digitsSum "d321as231dSdadaSA"  -- -> 12
    -- print $ digitsSum' "d321as231dSdadaSA" -- -> 12
    
    -- print $ capitalize "Abcd" -- -> "ABCD"
    -- print $ isCapitalized "ABCD" -- -> True
    -- print $ isCapitalized "AbCD" -- -> False
    
    -- print $ nOrMoreVowels ["cat", "dog", "doggo", "kitten", "rat"] 2 -- -> ["doggo", "kitten"]

    -- print $ isPrefixOf' "cat" "dog"  -- -> False
    -- print $ isPrefixOf' "dogs" "dog" -- -> False
    -- print $ isPrefixOf' "dog" "dog"  -- -> True
    -- print $ isPrefixOf' "cat" "cats" -- -> True
    
    -- print $ tails' [1,2,3,4] -- -> [[1,2,3,4],[2,3,4],[3,4],[4],[]]
    -- print $ isInfixOf'' "can" "I can't" -- -> True
    -- print $ isInfixOf'' "can't" "I can" -- -> False

    -- print $ longestSubstring  "111228888236555" -- -> 4 (заради 8888)

    -- print $ tighten "  This   is  a   sentence    " -- -> "This is a sentence"
    -- print $ words "  This   is  a   sentence    "   -- -> ["This","is","a","sentence"]
    -- print $ unwords ["This","is","a","sentence"]    -- -> "This is a sentence"
