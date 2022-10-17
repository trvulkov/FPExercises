-- Задача 1. Да се дефинира функция rotate :: Int -> [a] -> [a], която получава цяло число n и списък xs и "завърта" xs с n позиции наляво, 
-- т.е. елементите на xs се преместват с n позиции наляво, като тези, които при преместването излизат извън списъка, се добавят на края му. 
-- При подаване на отрицателно число n, завъртането е надясно с абсолютната стойност на n.
rotate :: Int -> [a] -> [a]
rotate n xs = if n >= 0 then drop len xs ++ take len xs else rotate (len + length xs) xs
    where len = n `mod` length xs

-- Задача 1. Да се дефинира функция dropEvery :: Int -> [a] -> [a], която приема естествено число n и списък xs и премахва всеки n-ти елемент на списъка.
dropEvery :: Int -> [a] -> [a]
dropEvery _ [] = []
dropEvery n xs = take (n-1) xs ++ dropEvery n (drop n xs)

-- Задача 2. Дефинирайте функция rf :: (Int -> Int) -> (Int -> Int) -> ([Int] -> (Int -> Int) -> [Int]), която има два аргумента - едноместните целочислени функции f и g.
-- Функцията rf трябва да върне нова двуаргументна функция с първи аргумент - списък от цели числа ns и втори аргумент - едноместна целочислена функция h.
-- Върнатата функция трябва да върне списък със всички числа h(n), за които е вярно, че n e от ns и f(n) > g(n).
rf :: (Int -> Int) -> (Int -> Int) -> ([Int] -> (Int -> Int) -> [Int])
rf f g = \ ns h -> [h n | n <- ns, f n > g n] 

-- Задача 3. Ако A(x1,y1) и B(x2,y2) са две точки в декартовата равнина – такива, че x1 ≠ x2, то уравнението на правата AB, която минава през тези две точки, е
-- y = f(x), където f(x) = y1 + (x-x1)*(y2-y1)/(x2-x1).
-- а) Нека имаме тип точка, представен така:
type Point = (Double, Double)
-- Да се дефинира функция line :: Point -> Point -> (Double -> Double), която по две точки връща функцията, определяща уравнението на минаващата през тях права.
line :: Point -> Point -> (Double -> Double)
line (x1, y1) (x2, y2) = \ x -> y1 + (x - x1) * (y2 - y1) / (x2 - x1)
-- б) Напишете функция от по-висок ред liesOn :: (Double -> Double) -> (Point -> Bool), която за дадена функция f, определяща уравнението на права, връща като резултат функция, която по дадена точка P(x,y)
-- проверява дали точката P лежи на правата f (дали y = f(x)).
liesOn :: (Double -> Double) -> (Point -> Bool)
liesOn f = \ (x, y) -> y == f x -- тъй като при сметките с числа с плаваща точка може да станат грешки, по-добър вариант е abs (y - (f x)) < EPS, където EPS е много малко число, например 1e-6

diagonal = line (0,0) (1,1)
onDiag = liesOn diagonal

-- Задача 4. Нека е дадено следното представяне на двоично дърво:
data BTree = Empty | Node Int BTree BTree
-- Да се дефинира функция deepestLeavesSum :: BTree -> Int, която връща сумата на най-дълбоките (най-отдалечените от корена) възли в дадено двоично дърво.
t4 :: BTree                                      --       1
t4 = Node 1 (Node 2 (Node 4 (Node 7 Empty Empty) --      / \
                            Empty)               --     2   3
                    (Node 5 Empty                --    / \   \
                            Empty))              --   4   5   6
            (Node 3 Empty                        --  /         \
                    (Node 6 Empty                -- 7           8
                            (Node 8 Empty Empty)))
                                            --       1
t5 :: BTree                                 --      / \
t5 = Node 1 (Node 2 (Node 4 Empty Empty)    --     2   3
                    Empty)                  --    /
            (Node 3 Empty Empty)            --   4

height :: BTree -> Int
height Empty               = 0
height (Node _ left right) = 1 + max (height left) (height right)

deepestLeavesSum :: BTree -> Int
deepestLeavesSum tree = helper tree (height tree)
    where
        helper Empty _               = 0
        helper (Node x _ _) 1        = x
        helper (Node _ left right) i = helper left (i-1) + helper right (i-1)


main :: IO()
main = do
    -- print $ rotate 5     ['a','b','c','d','e','f','g','h'] -- -> "fghabcde"
    -- print $ rotate 7     ['a','b','c','d','e','f','g','h'] -- -> "habcdefg"

    -- print $ rotate 8     ['a','b','c','d','e','f','g','h'] -- -> "abcdefgh"
    -- print $ rotate (-8)  ['a','b','c','d','e','f','g','h'] -- -> "abcdefgh"

    -- print $ rotate 3     ['a','b','c','d','e','f','g','h'] -- -> "defghabc"
    -- print $ rotate 11    ['a','b','c','d','e','f','g','h'] -- -> "defghabc"
    -- print $ rotate (-5)  ['a','b','c','d','e','f','g','h'] -- -> "defghabc"

    -- print $ rotate (-2)  ['a','b','c','d','e','f','g','h'] -- -> "ghabcdef"
    -- print $ rotate (-10) ['a','b','c','d','e','f','g','h'] -- -> "ghabcdef"


    -- print $ dropEvery 3 [1,2,3,1,2,3,1,2,3] -- -> [1,2,1,2,1,2]
    -- print $ dropEvery 4 [1,2,3,4,5,6,7,8,9] -- -> [1,2,3,5,6,7,9]


    -- print $ (rf ((-) (- 4)) (* (-2))) [1..10] (*3) -- -> [15,18,21,24,27,30]

    -- print $ diagonal 5.5 -- -> 5.5
    -- print $ diagonal 0.5 -- -> 0.5
    -- print $ onDiag (5.5, 5.5) -- -> True
    -- print $ onDiag (0.5, 0)   -- -> False

    -- print $ deepestLeavesSum t4 -- -> 15 (7 + 8)
    -- print $ deepestLeavesSum t5 -- -> 4
