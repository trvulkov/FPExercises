import Data.List(partition)

-- Задача 1. Чрез използването на n кубове е построена сграда. Кубът, намиращ се най-отдолу, т.е. основата, е с обем n^3. Кубът, който е върху него е с обем (n - 1)^3.
-- Кубът, поставен най-отгоре, има обем 1^3. Обемът на цялата сграда е: n^3 + (n-1)^3 + ... + 1^3 = m. 
-- Да се дефинира функция findNb :: Integer -> Integer, която по дадено m да връща броя кубове n, нужни за построяване на сградата. Ако n не съществува, да се връща -1.
findNb :: Integer -> Integer
findNb m = helper 1 m
    where
        helper i curr
            | curr < 0        = -1
            | curr == i^3     = i
            | otherwise       = helper (i + 1) (curr - i^3)

-- Задача 2. Нека са дадени две едноаргументни числови функции f и g и списък от числени стойности xs. Ще казваме, че функцията f доминира g върху множеството xs, ако за всяко x ∈ xs е вярно, че |f(x)| ≥ |g(x)|.
-- Дефинирайте функцията dominates f g xs, която връща резултата от проверката дали функцията f доминира g върху множеството xs.
dominates :: (Int -> Int) -> (Int -> Int) -> [Int] -> Bool
dominates f g xs = and [abs (f x) >= abs (g x) | x <- xs]

-- Задача 3. Нека имаме type Point = (Double, Double), представящ точка с реални координати. 
-- Да се дефинира функция splitPoints :: Point -> Double -> [Point] -> ([Point], [Point]), която приема точка p, радиус r и списък от точки ps и връща като резултат двойка от списъци,
-- като първият съдържа тези точки от ps, които са в кръга с център p и радиус r, а вторият - всички останали точки от ps.
type Point = (Double, Double)

splitPoints :: Point -> Double -> [Point] -> ([Point], [Point])
splitPoints center radius points = partition inCircle points
    where 
        inCircle point = distance center point <= radius
        distance (x1, y1) (x2, y2) = sqrt ((x1 - x2)^2 + (y1 - y2)^2)

-- Задача 4. Нека е дадено следното представяне на двоично дърво:
data BTree = Empty | Node Int BTree BTree
-- Да се дефинира функция isBinarySearchTree :: BTree Int -> Bool, която проверява дали подадено двоично дърво от цели числа е двоично дърво за търсене. 
-- Казваме, че едно двоично дърво е двоично дърво за търсене, ако лявото му поддърво съдържа само възли със стойности по-малки от тази във върха, 
-- а дясното му поддърво - само стойности по-големи или равни на тази във върха. Освен това, трябва и самите поддървета също да са двоични дървета за търсене.
t1 :: BTree                                 --      8
t1 = Node 8 (Node 3 (Node 1 Empty Empty)    --    /   \
                    (Node 4 Empty Empty))   --   3     10
            (Node 10 (Node 9 Empty Empty)   --  / \    / \
                     (Node 14 Empty Empty)) -- 1   4  9   14

t2 :: BTree                                 --      8
t2 = Node 8 (Node 3 (Node 1 Empty Empty)    --    /   \
                    (Node 4 Empty Empty))   --   3     10
            (Node 10 (Node 5 Empty Empty)   --  / \    / \
                     (Node 14 Empty Empty)) -- 1   4  5   14

t3 :: BTree                                 --      8
t3 = Node 8 (Node 3 (Node 5 Empty Empty)    --    /   \
                    (Node 6 Empty Empty))   --   3     10
            (Node 10 (Node 9 Empty Empty)   --  / \    / \
                     (Node 14 Empty Empty)) -- 5   6  9   14

isBinarySearchTree :: BTree -> Bool
isBinarySearchTree Empty               = True
isBinarySearchTree (Node x left right) = allNodes (< x) left && allNodes (>= x) right && isBinarySearchTree left && isBinarySearchTree right
    where
        allNodes _         Empty               = True
        allNodes predicate (Node x left right) = if predicate x then allNodes predicate left && allNodes predicate right else False

-- втори вариант - с ползване на inorder. В двоично наредено дърво, inorder обхождане трябва да даде стойности в нарастващ ред
inorder :: BTree -> [Int]
inorder Empty               = []
inorder (Node x left right) = inorder left ++ [x] ++ inorder right

isSorted xs = and $ zipWith (<) xs (tail xs)

isBinarySearchTree' :: BTree -> Bool
isBinarySearchTree' tree = isSorted $ inorder tree


main :: IO()
main = do
    -- print $ findNb 1071225          -- -> 45
    -- print $ findNb 40539911473216   -- -> 3568
    -- print $ findNb 135440716410000  -- -> 4824
    -- print $ findNb 4183059834009    -- -> 2022
    -- print $ findNb 91716553919377   -- -> -1
    -- print $ findNb 24723578342962   -- -> -1

    -- print $ dominates (+4) (*2) [1..4] -- -> True
    -- print $ dominates (+4) (*2) [1..5] -- -> False, заради 5: 5+4=9 < 5*2=10

    -- print $ splitPoints (1,1) 5 [(1,2),(2,3),(10,15),(-1,1),(12,14)]   -- -> ([(1.0,2.0),(2.0,3.0),(-1.0,1.0)],[(10.0,15.0),(12.0,14.0)])
    -- print $ splitPoints (10,10) 5 [(1,2),(2,3),(10,15),(-1,1),(12,14)] -- -> ([(10.0,15.0),(12.0,14.0)],[(1.0,2.0),(2.0,3.0),(-1.0,1.0)])
    -- print $ splitPoints (0, 0) 2 [(0, 0), (1, 1), (2, 2), (0, 2)]      -- -> ([(0.0,0.0),(1.0,1.0),(0.0,2.0)],[(2.0,2.0)])

    -- print $ isBinarySearchTree t1 -- -> True
    -- print $ isBinarySearchTree t2 -- -> False (в дясното поддърво има стойности, по-малки от корена)
    -- print $ isBinarySearchTree t3 -- -> False (лявото поддърво не е BST)
