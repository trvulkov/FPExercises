import Data.Char(isLower, isUpper, toLower, toUpper, ord)
import Data.List(nub, sort, group, groupBy, maximumBy)

-- Задача 1. Да се напише на езика Haskell функция reverseOrdSuff :: Int -> Int, която по дадено естествено число k намира число, получено от цифрите на най-дългия строго низходящ суфикс на k,
-- взети в обратен ред.
reverseOrdSuff :: Int -> Int
reverseOrdSuff n = listToInt $ increasingPrefix $ intToListInReverse n -- низходящ суфикс, взет в обратен ред === възходящ префикс на цифрите в обратен ред
    where
        intToListInReverse n = if n < 10 then [n] else n `mod` 10 : intToListInReverse (n `div` 10)

        increasingPrefix []         = []
        increasingPrefix [x]        = [x]
        increasingPrefix (x1:x2:xs) = if (x1 >= x2) then [x1] else x1 : increasingPrefix (x2:xs)

        listToInt xs = foldl (\ num rest -> 10*num + rest) 0 xs
        listToInt' xs = (read . concatMap show) xs -- xs е списък от цифри. show в този случай ще конвертира числа към низове, а concatMap ще приложи show към всеки елемент на xs,
        -- като така ще се получи списък от низове (всеки представляващ една цифра), и след това ще ги конкатенира, като така ще се получи низ от цифрите на числото.
        -- Накрая read ще конвертира този низ към самото число.

-- Задача 2. Да се напише на Haskell функция sumUnique :: [[Int]] -> Int, която по списък от списъци от цели числа намира сумата на тези от числата, които са уникални в рамките на списъка, в който се срещат.
sumUnique :: [[Int]] -> Int
sumUnique xss = sum $ concat [uniques xs []| xs <- xss]
    where
        uniques [] checked     = []
        uniques (x:xs) checked = if notElem x xs && notElem x checked then x : uniques xs (x:checked) else uniques xs (x:checked)

sumUnique' :: [[Int]] -> Int
sumUnique' = sum . concat . map unique
    where unique = concat . filter ((==1) . length) . group . sort -- намираме елементите които се срещат точно веднъж в списъка - първо сортираме, за да може елементите които се срещат многократно да
    -- са подредени, така че group да върни списък от списъци, всеки от които съдържа всичките срещания на даден елемент. Чрез filter взимаме само спъсицте на елементи които се срещат само веднъж,
    -- и накрая с concat получаваме списък от тях

-- Задача 3 -> Задачи 11 и 12 от Упражнение 11.

-- Задача 4. Нека е даден списък от точки в тримерно пространство, представен като списък от наредени тройки.Да се напише на Haskell функция minDistance :: [(Double,Double,Double)] -> Double,
-- която намира най-малкото от разстоянията между двойките точки от списъка. Разстоянието d се дeфинира по следния начин: ако разглеждаме точките p1=(x1, y1, z1) и p2=(x2, y2, z2), то
-- d(p1, p2) = (x1-x2)*(x1-x2)+(y1-y2)*(y1-y2)+(z1-z2)*(z1-z2).
minDistance :: [(Double,Double,Double)] -> Double
minDistance xs = minimum [distance p1 p2 | p1 <- xs, p2 <- xs, p1 /= p2] -- изграждаме списък от всички двойки различни точки, за всяка двойка изчисляваме съответното разстояние, и взимаме минималното
    where distance (x1,y1,z1) (x2,y2,z2) = (x1-x2)*(x1-x2) + (y1-y2)*(y1-y2) + (z1-z2)*(z1-z2)

-- Задача 5. Да се напише на Haskell функция reduceStr str, която за даден символен низ str връща негов редуциран вариант, получен в резултат на последователно взаимно унищожаване на двойки съседни знакове,
-- които представляват съответно главен и малък (или малък и главен) вариант на една и съща буква от латиницата. Правилото за взаимно унищожаване на съответни главни и малки (или малки и главни) букви се
-- изпълнява многократно, докато е възможно, върху резултата от последното му прилагане. Всички останали знакове в низа остават непроменени.
reduceStr :: String -> String
reduceStr str = if str == reduced then str else reduceStr reduced
    where
        reduced = reduce str []

        reduce []  result = result
        reduce [c] result = result ++ [c]
        reduce (c1:c2:str) result
            | isLower c1 && isUpper c2 && c1 == toLower c2 = result ++ str
            | isUpper c1 && isLower c2 && c1 == toUpper c2 = result ++ str
            | otherwise                                    = reduce (c2:str) (result ++ [c1])

reduceStr' :: String -> String
reduceStr' str = if str == reduced then str else reduceStr' reduced
    where
        reduced = reduce str

        diff = ord 'a' - ord 'A'
        reduce = concat . (filter ((==1) . length)) . (groupBy (\ a b -> abs (ord a - ord b) == diff))
        -- groupBy работи подобно на group, но не сравнява елементите директно по (==), а по подаден от нас предикат. В този случай смятаме две букви за равни, ако са на разстояние diff една от друга - 
        -- това е разстоянието между малка и главна буква в ASCII таблицата. Тоест след групирането ще имаме списък от списъци, който съдържа двуелементни списъци от двойките които искаме да унищожим,
        -- и едноелементни списъци от останалите букви. Чрез filter махаме двойките, и чрез concat получаваме обикновен низ.

-- Задача 6. Да се дефинира функция maximize :: (Ord a, Num a) => [(a -> a)] -> (a -> a), за която оценката на обръщението maximize l, където l е непразен списък от едноместни числови функции, да е
-- едноместна числова функция на аргумент x, която дава стойността f(x) на тази фунция f от списъка l, за която числото f(x) е най-голямо по абсолютна стойност
maximize :: (Ord a, Num a) => [(a -> a)] -> (a -> a)
maximize fs = \x -> snd $ maximumBy compareFunctions [(abs (f x), f x) | f <- fs]
    where compareFunctions v1@(abs1, _) v2@(abs2, _) = compare abs1 abs2

-- Задача 7. Функцията g е обратна на функцията f в дадено множество А, ако f . g = id в A и g . f = id в A. Да се напише на езика Haskell функция inverseFun :: (Int -> Int) -> (Int -> Int) -> Int -> Int -> Bool,
-- която за дадени целочислени функции f и g връща True точно когато g е обратна на f в даден целочислен интервал [a, b].
inverseFun :: (Int -> Int) -> (Int -> Int) -> Int -> Int -> Bool
inverseFun f g a b
    | a > b                            = True
    | (f . g) a == a && (g . f) a == a = inverseFun f g (a + 1) b
    | otherwise                        = False
 
inverseFun' :: (Int -> Int) -> (Int -> Int) -> Int -> Int -> Bool
inverseFun' f g a b = map (f . g) interval == interval && map (g . f) interval == interval
    where interval = [a..b]

inverseFun'' :: (Int -> Int) -> (Int -> Int) -> Int -> Int -> Bool
inverseFun'' f g a b = all (\ x -> (f . g) x == x && (g . f) x == x) [a..b]


-- Задача 8. Нека е дефиниран алгебричен тип
data BTree = NullT | Node (Float,Float) BTree BTree --, който се използва за представяне на двоично дърво от двойки от реални числа, задаващи начала и краища на числови интервали
-- (предполага се, че първият елемент на всяка двойка е по-малък от нейния втори елемент). Напишете на езика Haskell функция orderedTree tree, която проверява дали дадено двоично дърво tree от тип BTree
-- е наредено относно релацията „подинтервал”.
tree1 = Node (3.0,10.0) (Node (5.0,8.0) (Node (6.0,7.0) NullT NullT)    --       (3,10)
                                        (Node (4.0,9.0) NullT NullT))   --       /    \
                        (Node (2.0,12.0) NullT                          --    (5,8)  (2,12)
                                        (Node (1.0,15.0) NullT NullT))  --    /  \      \
                                                                        -- (6,7) (4,9) (1,15)
 
tree2 = Node (3.0,10.0) (Node (5.0,8.0) (Node (6.0,7.0) NullT NullT)    --       (3,10)
                                        (Node (7.0,9.0) NullT NullT))   --       /    \
                        (Node (2.0,12.0) NullT                          --    (5,8)  (2,12)
                                        (Node (1.0,15.0) NullT NullT))  --    /  \      \
                                                                        -- (6,7) (7,9) (1,15)

subInterval :: (Float, Float) -> (Float, Float) -> Bool
subInterval (a1, b1) (a2, b2) = a2 <= a1 && b1 <= b2 -- проверка дали първият аргумент е подинтервал на втория

orderedTree :: BTree -> Bool
orderedTree NullT                                                                           = True
orderedTree (Node _            NullT                        NullT)                          = True
orderedTree (Node rootInterval left@(Node leftInterval _ _) NullT)                          = leftInterval `subInterval` rootInterval  && orderedTree left
orderedTree (Node rootInterval NullT                        right@(Node rightInterval _ _)) = rootInterval `subInterval` rightInterval && orderedTree right
orderedTree (Node rootInterval left@(Node leftInterval _ _) right@(Node rightInterval _ _)) = leftInterval `subInterval` rootInterval && rootInterval `subInterval` rightInterval && 
                                                                                              orderedTree left && orderedTree right


main :: IO()
main = do
    -- print $ reverseOrdSuff 37563 -- -> 36
    -- print $ reverseOrdSuff 32763 -- -> 367
    -- print $ reverseOrdSuff 32567 -- -> 7
    -- print $ reverseOrdSuff 32666 -- -> 6
    
    -- print $ sumUnique [[1,2,3,2],[-4,-4],[5]]   -- -> 9 (= 1+3+5)
    -- print $ sumUnique [[2,2,2],[3,3,3],[4,4,4]] -- -> 0
    -- print $ sumUnique [[1,2,3],[4,5,6],[7,8,9]] -- -> 45
        
    -- print $ minDistance [(1,1,1), (1,2,1), (2,2,2)]                -- -> 1.0
    -- print $ minDistance [(0,0,0), (1,1,1), (2,2,2), (0.2,0.2,0.2)] -- -> 0.12000000000000002
    
    -- print $ reduceStr' "dabAcCaCBAcCcaDD" -- -> "dabCBAcaDD"
    -- print $ reduceStr' "abcdDCBA"         -- -> 
    -- print $ reduceStr' "aabbccddAABBCCDD" -- -> "aabbccddAABBCCDD"
    
    -- print $ (maximize [(\x -> x*x*x),(\x -> x+1)]) 0.5  -- -> 1.5
    -- print $ (maximize [(\x -> x*x*x),(\x -> x+1)]) (-2) -- -> -8.0
    
    -- print $ inverseFun (\x -> x+1) (\x -> x-1) 5 10 -- -> True
    -- print $ inverseFun (\x -> x*x) (\x -> x^3) 0 1  -- -> True
    -- print $ inverseFun (\x -> x+1) (\x -> x+2) 0 1  -- -> False
    
    -- print $ orderedTree tree1 -- -> True
    -- print $ orderedTree tree2 -- -> False
