import Data.List(minimumBy, nub, group, sort, sortBy)
import Data.Char(digitToInt, ord)
import Data.Function(on)

-- Вектори/n-торки (tuples) - наредени n-торки от елементи. Типовете им са определи предварително, но могат да бъдат различни.
a :: (Int, Int)
a = (1, 2)
-- може да достъпваме първи и втори елемент с fst и snd съответно (само при двойки)
b :: (Int, Float, Float)
b = (1, 2.3, 4.5)

-- може да ги сравняваме с ==, но само ако са с еднаква дължина

-- може да ги подаваме като аргументи
addPair :: (Int, Int) -> Int
addPair p = fst p + snd p
-- може да ползваме pattern matching с n-торки
addPair' :: (Int, Int) -> Int
addPair' (x, y) = x + y
-- функциите fst и snd ги има само за наредени двойки - за n-торки с по-голямо n начинът да достъпим компонентите им е чрез pattern matching

-- може да ги връщаме като резултат на функция
divide :: Int -> Int -> (Int, Int)
divide x y = (x `div` y, x `mod` y) -- ще върне (цяла част, остатък)

-- ако някоя координата не ни интересува в даден случай, може в pattern-а да я заместим с '_' - wildcard
addTriple :: (Int, Int, Int) -> Int
addTriple (x, _, z) = x + z


-- може да дефинираме наши типове, като имената им трябва да започват с главна буква
type Grade = (String, String, Float)
grade1 :: Grade
grade2 :: Grade
grade1 = ("John", "Algebra", 4.75)
grade2 = ("Jane", "Geometry", 5.25)


-- Задача 0. Да се дефинира функция isAscending :: Integer -> Bool, която проверява дали цифрите на число са във възходящ ред. Функцията да получава число, но да работи със списък от цифрите му.
isAscending :: Integer -> Bool
isAscending num = all ordered $ zip digits (tail digits)
    where
        digits = map digitToInt $ show num -- show е вградена функция, която конвертира нещо до низ (т.е. списък от Char-ове), а digitToInt (от Data.Char) конвертира символ на цифра до тази цифра като Int
        ordered (x, y) = x <= y
       -- като zip-нем списъка със същия, но "отместен" наляво (чрез взимане на опашката), получаваме списък от двойки последователни елементи, например:
       -- [1,2,3,4,...]
       -- [2,3,4,...]
       -- [(1,2),(2,3),(3,4),...]
       -- с all ordered провяваме дали при всяка двойка първият елемент е по-малък от вторият, тоест дали наистина всяка цифра е по-малка от следващата

-- В горния вариант първо конструираме двойките последователни елементи, и след това сравняваме съответните елементи. Може да направим двете неща с една стъпка, като вместо zip ползваме zipWith (<).
isAscending' :: Integer -> Bool
isAscending' num = and $ zipWith (<) digits (tail digits)
    where digits = map digitToInt $ show num

-- Задача 1. Да се дефинира функция numBiggerElements :: [Int] -> [(Int, Int)], която за даден списък от числа xs връща като резултат 
-- списък с елементи от вида (xi, ni), където xi е i-тият елемент на xs, а ni е броят на елементите на xs, които са по-големи от xi.
numBiggerElements :: [Int] -> [(Int, Int)]
numBiggerElements xs = [(x, length [y | y <- xs, y > x]) | x <- xs]

-- Задача 2. Да се дефинира функция splitByParity :: [Int] -> ([Int], [Int]), която получава списък от цели числа и го разделя на два списъка - от нечетни и четни.
splitByParity :: [Int] -> ([Int], [Int])
splitByParity xs = (filter odd xs, filter even xs)

-- Задача 3. Да се дефинира функция partition' :: (a -> Bool) -> [a] -> ([a], [a]), която получава предикат и списък и разделя списъка на 2 части:
-- 1) елементите, които удовлетворяват предиката
-- 2) елементите, които не го удовлетворяват
-- Функцията е с ', тъй като вече съществува вградена функция под името partition.
partition' :: (a -> Bool) -> [a] -> ([a], [a])
partition' p xs = (filter p xs, filter (not . p) xs)

-- Да се пререши splitByParity, ползвайки partition.
splitByParity' :: [Int] -> ([Int], [Int])
splitByParity' xs = partition' odd xs

-- Задача 4. Да се дефинира функция quickSort :: [Int] -> [Int], която реализира бързо сортиране върху списък.
quickSort :: [Int] -> [Int]
quickSort []     = []
quickSort (p:xs) = quickSort smaller ++ [p] ++ quickSort larger
    where (smaller, larger) = partition' (<p) xs

-- Задача 5. Да се дефинира тип Vector, определящ се от три координати - x, y и z. Да се дефинират функции за работа с типа:
type Vector = (Double, Double, Double)
-- а) sumVectors :: Vector -> Vector -> Vector, която намира сумата на два вектора
sumVectors :: Vector -> Vector -> Vector
sumVectors (x1, y1, z1) (x2, y2, z2) = (x1 + x2, y1 + y2, z1 + z2)
-- b) scaleVector :: Vector -> Double -> Vector, която умножава скалар и вектор
scaleVector :: Vector -> Double -> Vector
scaleVector (x, y, z) p = (p * x, p * y, p * z)
-- c) dotProduct :: Vector -> Vector -> Double, която намира скаларното произведение на два вектора
dotProduct :: Vector -> Vector -> Double
dotProduct (x1, y1, z1) (x2, y2, z2) = x1 * x2 + y1 * y2 + z1 * z2

v, p:: Vector
v = (1, 1, 1)
p = (2, 3, 5)

-- Задача 6. Да се дефинира тип Product, определящ се от име, количество и цена. Да се дефинира тип Shop (“база от данни”), който представлява инвентара от продукти на даден магазин.
type Product = (String, Int, Float)
type Shop = [Product]

p1, p2, p3, p4 :: Product
p1 = ("Milk", 5, 1.20)
p2 = ("Cheese", 20, 1.80)
p3 = ("Bread", 10, 0.50)
p4 = ("Chocolate", 3, 2.00)

shop :: Shop
shop = [p1, p2, p3, p4]

-- Задача 7. Да се дефинира функция getPrice :: Product -> Float, която връща цената на даден продукт.
getPrice :: Product -> Float
getPrice (_, _, price) = price

-- Задача 8. Да се дефинира функция getTotal :: Shop -> Float, която връща оборота на даден магазин, ако е продаден целият инвентар.
getTotal :: Shop -> Float
getTotal [] = 0
getTotal ((_, quantity, price) : xs) = (fromIntegral quantity) * price + getTotal xs -- тъй като quantity е Int, а price е Float, се налага конвертиране чрез fromIntegral

-- 2ри вариант - с map и sum
getTotal' :: Shop -> Float
getTotal' xs = sum $ map (\ (_, quantity, price) -> (fromIntegral quantity) * price) xs 

-- Задача 9. Да се дефинира функция buy :: String -> Int -> Shop -> Shop, която симулира “закупуването” на даден продукт, като приема име, количество и магазин. Да се вземе предвид, че не може след продажбата 
-- в магазина да имаме отрицателно количество за даден продукт. Ако искаме да купим продукт, но неговата наличност е недостатъчна, нека операцията да е празна, т.е. да не променя нищо.
-- Ако след покупка количеството е станало 0, продуктът да се премахне от инвентара.
buy :: String -> Int -> Shop -> Shop
buy _ _ [] = error "No such product"
buy toBuyName toBuyQuantity (x@(name, quantity, price) : xs) -- синоними с @ може да правим и при n-торки - така може да реферираме както до цялата n-торка като x, така и до отделните ѝ елементи с техните имена
    | toBuyName == name && toBuyQuantity < quantity  = (name, quantity - toBuyQuantity, price) : xs -- намаляме количеството
    | toBuyName == name && toBuyQuantity == quantity = xs                                           -- премахваме продукта изцяло
    | toBuyName == name && toBuyQuantity > quantity  = x : xs                                       -- не променяме нищо
    | otherwise                                      = x : buy toBuyName toBuyQuantity xs           -- продължаваме да търсим в магазина

-- Задача 10. Да се дефинира функция getNeeded :: Int -> Shop -> [Product], която връща списък от продукти, чиято наличност е по-малка или равна на даден праг (количество).
getNeeded :: Int -> Shop -> [Product]
getNeeded _ [] = []
getNeeded needed (x@(name, quantity, price) : xs)
    | quantity <= needed = x : getNeeded needed xs
    | otherwise          = getNeeded needed xs

-- 2ри вариант - filter
getNeeded' :: Int -> Shop -> [Product]
getNeeded' needed xs = filter ((<= needed) . getQuantity) xs
    where getQuantity (_, quantity, _) = quantity

-- подобно на горния, но с list comprehension вместо filter, благодарение на което не ни се налага да дефинираме функция getQuantity
getNeeded'' :: Int -> Shop -> [Product]
getNeeded'' needed xs = [x | x@(name, quantity, price) <- xs, quantity <= needed]

-- Задача 11. Да се дефинира функция closestToAverage :: Shop -> String, която намира името на продукта, чиято цена е най-близка до средната за всички в даден магазин.

-- първо - да намерим средната цена
getAverage :: Shop -> Float
getAverage xs = sum prices / fromIntegral (length prices)
    where prices = [price | (_, _, price) <- xs] -- map getPrice xs

-- вече към същинската функция
-- 1ви вариант - с fold-ване за намиране на минималния елемент
closestToAverage :: Shop -> String
closestToAverage xs = name
    where 
        (name, _, _) = foldl1 compareProducts xs -- Тъй като резултатът от функцията е 3ка, а на нас ни трябва само 1ва ѝ координата, си запазваме резултатът тук и извличаме само това, което ни е нужно от него.
        compareProducts p1@(_, _, price1) p2@(_, _, price2) = if abs (price1 - average) < abs (price2 - average) then p1 else p2
        average = getAverage xs
        -- Алтернативно - може първо да извадим средното от всяка цена, и после да сравняваме директно тези разлики:
        -- (name, _, _) = foldl1 compareProducts (map (substract average) xs)
        -- substract x (name, quantity, price) = (name, quantity, abs(price - x))
        -- compareProducts p1@(_, _, difference1) p2@(_, _, difference2) = if difference1 < difference2 then p1 else p2

-- 2ри вариант - с ползване на minimumBy, вградена функция от библиотеката Data.List, която намира минималния елемент на списък според подадена функция за сравнение.
-- Подадената функция не е от типа на < и >, които връщат булева стойност, а трябва да връща специалния тип Ordering.
-- Този тип има 3 валидни стойности - LT (less than), GT (greater than) и EQ (equal). По-нататък ще коментираме такива типове, за сега не са ни важни тези стойности, а функцията compare, която
-- приема два аргумента от един и същи тип (който поддържа сравнение), сравнява ги, и връща съответната стойност на Ordering (ако първият е по-малък, ще върне LT, ако са равни - EQ, иначе - GT).
closestToAverage' :: Shop -> String
closestToAverage' xs = name
    where
        (name, _, _) = minimumBy comparePrices xs -- За n-торки има дефиниран compare, но той ще ги сравни по всички координати, а ние искаме да направим по-различно сравнение 
        -- (интересува ни само третата координата, и трябва преди сравнението да приложим някои операции към нея), затова ще си дефинираме наша функция, която ще ползва вградения compare.
        comparePrices :: Product -> Product -> Ordering
        comparePrices (_, _, price1) (_, _, price2) = compare (abs(price1 - average)) (abs(price2 - average))
        
        average = getAverage xs


shop1, shop2 :: Shop
shop1 = [("bread", 1, 1), ("milk", 1, 2.5), ("lamb", 1, 10), ("cheese", 1, 5), ("butter", 1, 2.3)]
shop2 = [("bread", 1, 1), ("cheese", 1, 2.5), ("bread", 1, 1), ("cheese", 1, 5), ("butter", 1, 2.3)]

-- Задача 12. Да се дефинира функция cheaperAlternative, която намира броя на продуктите, за които има продукт със същото име, но по-ниска цена.
cheaperAlternative :: Shop -> Int
cheaperAlternative xs = length $ filter hasTwoPrices $ groupPrices xs
    where
        names = nub [name | (name, _, _) <- xs] -- nub е вградена ф-ия от библиотеката Data.List, която изтрива повторенията в списък. Тук ще получим списък от имената на продукти, без повторения

        groupPrices :: Shop -> [[Float]]
        groupPrices xs = [[price | (name', _, price) <- xs, name' == name] | name <- names] -- тук ще получим списък от списъци, всеки съдържащ различните цени на даден продукт
        -- например в shop2 ще имаме [[1.0,1.0],[2.5,5.0],[2.3]], т.к. bread присъства 2 пъти с цена 1, cheese - 2 пъти с цени 2.5 и 5.0, а butter - веднъж с 2.3  

        hasTwoPrices xs = length (nub xs) > 1 -- ако има две различни цени, то след като премахнем повторенията трябва списъкът да е с дължина поне 2



-- Задача 13. Да се дефинира функция calcFrequencyTable :: String -> [(Char,Int)], която получава низ cs, състоящ се от символите a-z. Функцията трябва да върне списък от двойки, в които
-- първият елемент е символ от cs, а вторият е броя на срещания на този символ. В резултатът трябва да присъстват всички символи на cs, без повторение. Двойките трябва да са сортирани
-- в низходящ ред по броя на срещанията, като при еднакъв броя срещания първа трябва да е двойката със символ с по-малък ASCII код.

-- 1ви вариант: със сортиране
calcFrequencyTable :: String -> [(Char, Int)]
calcFrequencyTable "" = []
calcFrequencyTable cs = sortBy (\(_,cnt1) (_,cnt2) -> compare cnt2 cnt1) [(c, length (filter (==c) cs)) | c <- sort (nub cs)]
-- sortBy е вградена функция, която получава функция за сравнение и сортира списъка според нея. Както minimumBy, тази функция трябва да връща стойност от типа Ordering.
-- Тук със sort (nub cs) получаваме списък от буквите в азбучен ред, след това конструираме списък от двойки с тях и съответните им броеве на срещания, и накрая със sortBy и подадената ламбда
-- сортираме двойките по вторите им координати. Възползваме се от това, че вградената сортировка е устойчива (стабилна). Това свойство ще го разглеждате по-детайлно по ДАА, но идеята е че когато две стойности
-- са равни, след сортировката те ще присъстват в същия ред както преди нея. В този конкретен случай, тъй като първоначалният списък от двойки е сортиран по азбучен ред, ако две двойки са равни
-- (тоест имат една и съща буква с един и същ брой срещания), то след сортировката те също ще са сортирани по азбучен ред, т.е. първа ще е тази с по-малък ASCII код - точно това се иска в условието.

-- сравняването по втора координата може да стане и без да се налага ламбда - чрез специалния оператор (on) може директно да приложим compare към вторите координати, но това ще ни даде
-- compare cnt1 cnt2, поради което се налага и функцията flip, която просто разменя аргументите
calcFrequencyTable' :: String -> [(Char, Int)]
calcFrequencyTable' "" = []
calcFrequencyTable' cs = sortBy (flip compare `on` snd) pairs
    where pairs = [(head x, length x) | x <- group (sort cs)]
    -- Тук вместо да ползваме nub, първо сортираме и после групираме, при което ще получим списък от списъци, всеки съдържащ срещанията на дадена буква. Този списък може да трансформираме до
    -- нужният ни списък от двойки (буква, брой срещания), като просто за всеки списък вземем главата му за буквата, и дължината му за броя срещания.

-- 2ри вариант: с добавяне на елементи към вече сортиран списък, запазвайки го сортиран (подобно на insertion sort)
calcFrequencyTable'' :: String -> [(Char, Int)]
calcFrequencyTable'' str = helper (nub str) [] -- взимаме списък от буквите без повторения, и за всяка буква добавяме съответстващата ѝ двойка на правилното място в списък 
    where
        helper []     pairs = pairs
        helper (c:cs) pairs = helper cs (insert (c, length (filter (==c) str)) pairs)
       
        insert p@(_,_) []                     = [p]
        insert p1@(_,cnt1) (p2@(_,cnt2) : ps) = if (compare p1 p2) then p1:p2:ps else p2 : insert p1 ps
        compare p1@(c1,cnt1) p2@(c2,cnt2)
            | cnt1 > cnt2                     = True
            | cnt1 == cnt2 && ord c1 < ord c2 = True
            | otherwise                       = False



main :: IO()
main = do
    -- print a       -- -> (1,2)
    -- print $ fst a -- -> 1
    -- print $ snd a -- -> 2
    -- print b       -- -> (1,2.3,4.5)
    -- print $ fst b -- Couldn't match expected type `(a0, b0)' with actual type `(Int, Float, Float)'
    
    -- print $ (1,2) == (1,2) -- -> True
    -- print $ (1,2) == (1,1) -- -> False
    -- print $ (1,2,3) == (1,2) -- error: Couldn't match expected type `(Integer, Integer, Integer)' with actual type `(Integer, Integer)'
    
    -- print $ addPair (3,4)  -- -> 7
    -- print $ addPair' (3,4) -- -> 7
    
    -- print $ divide 7 2 -- -> (3,1)
    
    -- print grade1 -- -> ("John","Algebra",4.75)
    -- print grade2 -- -> ("Jane","Geometry",5.25)
    
    -- print $ isAscending 234 -- -> True
    -- print $ isAscending' 243 -- -> False

    -- print $ numBiggerElements [5,6,3,4]    -- -> [(5,1),(6,0),(3,3),(4,2)]
    -- print $ splitByParity [1, 2, 3, 4]     -- -> ([1,3], [2,4])
    -- print $ partition' (<5) [0..10]        -- -> ([0,1,2,3,4], [5,6,7,8,9,10])
    -- print $ quickSort [3,2,5,1,6,10,8,9,4] -- -> [1,2,3,4,5,6,8,9,10]
    
    -- print $ sumVectors   (1, 1, 1) (2, 3, 5) -- -> (3.0,4.0,6.0)
    -- print $ scaleVector  (2, 3, 5) 3         -- -> (6.0,9.0,15.0)
    -- print $ dotProduct   (1, 1, 1) (2, 3, 5) -- -> 10.0
    

    -- print $ getPrice ("Milk", 5, 1.20) -- -> 1.2
    -- print shop -- -> [("Milk",5,1.2),("Cheese",20,1.8),("Bread",10,0.5),("Chocolate",3,2.0)]
    -- print $ getTotal shop     -- -> 53.0
    -- print $ buy "Milk" 3 shop -- -> [("Milk",2,1.2),("Cheese",20,1.8),("Bread",10,0.5),("Chocolate",3,2.0)]
    -- print $ buy "Milk" 5 shop -- -> [("Cheese",20,1.8),("Bread",10,0.5),("Chocolate",3,2.0)]
    -- print $ getNeeded 5 shop  -- -> [("Milk",5,1.2),("Chocolate",3,2.0)]
    
    -- print shop1 -- -> [("bread",1,1.0),("milk",1,2.5),("lamb",1,10.0),("cheese",1,5.0),("butter",1,2.3)]
    -- print shop2 -- -> [("bread",1,1.0),("cheese",1,2.5),("bread",1,1.0),("cheese",1,5.0),("butter",1,2.3)]
    -- print $ getAverage shop1         -- -> 4.16
    -- print $ closestToAverage shop1   -- -> "cheese"
    -- print $ closestToAverage' shop1  -- -> "cheese"
    -- print $ cheaperAlternative shop2 -- -> 1 (поради cheese, което присъства с цени 2.5 и 5.0


    -- print $ calcFrequencyTable "ababac"    -- -> [('a',3),('b',2),('c',1)]
    -- print $ calcFrequencyTable "aaabbbc"   -- -> [('a',3),('b',3),('c',1)]
    -- print $ calcFrequencyTable "ababacccc" -- -> [('c',4),('a',3),('b',2)]
    -- print $ calcFrequencyTable "bababa"    -- -> [('a',3),('b',3)]
