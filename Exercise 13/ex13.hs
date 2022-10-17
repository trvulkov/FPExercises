import Data.List(delete, intersect)
-- Задача 0. Миналият път видяхме как да представим аритметичен израз чрез двоично дърво и как да го отпечатаме. Как може да изчислим стойността му?
-- Първо, за да бъде по-ясно кои върхове са листа (и съответно трябва да не съдържат операции, а стойности), може да дефинираме типа двоично дърво така:
data BTree a = Empty |
               Leaf a |
               Node a (BTree a) (BTree a) deriving Show
                                                    
expr1 :: BTree Char                                 --      -
expr1 = Node '-' (Node '+' (Leaf '5')               --     / \
                           (Node '*' (Leaf '2')     --    +   4
                                     (Leaf '5')))   --   / \
                 (Leaf '4')                         --  5   *
                                                    --     / \
                                                    --    2   5
getExpression :: BTree Char -> String
getExpression Empty                = []
getExpression (Leaf c)             = [c]
getExpression (Node c left right)  = "(" ++ (getExpression left) ++ [c] ++ (getExpression right) ++ ")"

-- Вече към изчисляването:
-- 1ви вариант - по-специален вид дърво, с възможност за различен тип стойности по вътрешните възли и листата
data ExprTree node leaf = EmptyExpr |
                          ExprLeaf leaf |
                          ExprNode node (ExprTree node leaf) (ExprTree node leaf) deriving Show
-- това дърво вече има два типа - node и leaf (можеха да бъдат просто a и b, но типовите променливи могат да имат и по-дълги и описателни имена), като възлите имат стойност от първия тип, а листата от втория

expr2 :: ExprTree Char Double                                   --      -
expr2 = ExprNode '-' (ExprNode '+' (ExprLeaf 5)                 --     / \
                                   (ExprNode '*' (ExprLeaf 2)   --    +   4
                                                 (ExprLeaf 5))) --   / \
                     (ExprLeaf 4)                               --  5   *
                                                                --     / \
                                                                --    2   5                                                    

calc :: ExprTree Char Double -> Double
calc EmptyExpr    = 0
calc (ExprLeaf x) = x -- при листо няма какво да смятаме - трябва просто да върнем стойността, която стои в самото листо
calc (ExprNode op left right)
    | op == '+' = calc left + calc right
    | op == '-' = calc left - calc right
    | op == '*' = calc left * calc right
    | op == '/' = calc left / calc right
    | op == '^' = calc left ** calc right
    | otherwise = error "Invalid tree"

-- 2ри вариант - дърво от низове и ползване на функцията read
expr3 :: BTree String
expr3 = Node "-" (Node "+" (Leaf "5")
                           (Node "*" (Leaf "2") 
                                     (Leaf "5")))
                 (Leaf "4")

calc' :: BTree String -> Double
calc' Empty    = 0
calc' (Leaf x) = read x 
calc' (Node op left right)
    | op == "+" = calc' left + calc' right
    | op == "-" = calc' left - calc' right
    | op == "*" = calc' left * calc' right
    | op == "/" = calc' left / calc' right
    | op == "^" = calc' left ** calc' right
    | otherwise = error "Invalid tree"
-- read е нещо като обратната функция на show - докато show взима стойност и я конвертира в низ (който после може да отпечатаме на конзолата чрез print), read взима низ и го конвертира до съответната стойност.
-- Тук във втория случай на функцията calc' ползваме read - благодарение на сигнатурата на функцията компилатора знае че трябва да се върне Double, и така преобразува низа в стойност от тип Double.

-- Задача 1. Нека имаме типът за цвят 
data Color = Red | Green | Blue deriving (Read, Show, Eq)
-- Дефинирайте функция maxDepthBlueNode :: BTree Color -> Int, която намира дълбочината на най-дълбокия (най-отдалечения от корена) връх с цвят Blue на дадено двоично дърво от тип Color.
colorTree :: BTree Color                                            --            Blue
colorTree = Node Blue (Node Red (Node Green Empty Empty)            --           /    \
                                Empty)                              --        Red      Red
                      (Node Red (Node Blue (Node Green Empty Empty) --        /        /
                                           (Node Red Empty Empty))  --     Green     Blue
                                Empty)                              --               /   \
                                                                    --            Green  Red

maxDepthBlueNode :: BTree Color -> Int
maxDepthBlueNode tree  = helper tree 1
    where 
        helper Empty                  _         = 0
        helper (Node Blue left right) currDepth = max currDepth (max (helper left (currDepth + 1)) (helper right (currDepth + 1)))
        helper (Node _    left right) currDepth = max (helper left (currDepth + 1)) (helper right (currDepth + 1))

-- Задача 2. Дефинирайте функция maxDepthNode :: BTree Color -> Color -> Int, която намира дълбочината на най-дълбокия връх с цвят, подаден като аргумент, на дадено двоично дърво от тип Color. 
maxDepthNode :: BTree Color -> Color -> Int
maxDepthNode tree color = helper tree 1
    where 
        helper Empty _ = 0
        helper (Node c left right) currDepth 
            | c == color = max currDepth (max (helper left (currDepth + 1)) (helper right (currDepth + 1)))
            | otherwise  = max (helper left (currDepth + 1)) (helper right (currDepth + 1))

-- Задача 3. Да се дефинира алгебричен тип NTree а, който да представлява дърво с произволен брой наследника на всеки възел. За него да се дефинира фунцкия size, която брои елементите му
data NTree a = NEmpty | NNode a [(NTree a)]  
nTree1 :: NTree Int                               --       1
nTree1 = NNode 1 [(NNode 2 [(NNode 3 [NEmpty]),   --      / \
                            (NNode 4 [NEmpty]),   --     2   6
                            (NNode 5 [NEmpty])]), --    /|\  |
                  (NNode 6 [(NNode 7 [NEmpty])])] --   3 4 5 7

size :: NTree a -> Int
size NEmpty             = 0
size (NNode _ subTrees) = 1 + sum (map size subTrees)

-- Задача 4. Казваме, че едно дърво е грациозно, ако абсолютните стойности на разликите между стойностите на всеки елемент и бащиния му са четни. 
-- Да се дефинира функция isGraceful :: NTree Int -> Bool, която приема n-арно дърво и проверява дали то е грациозно.
nTree2 :: NTree Int             --     1
nTree2 = NNode 1 [NNode 3 [],   --  / / \ \
                  NNode 5 [],   -- 3 5   7 9
                  NNode 7 [],   
                  NNode 9 []]
                                        --   7
nTree3 :: NTree Int                     --   |  
nTree3 = NNode 7 [NNode 9 [NNode 5 [],  --   9
                           NNode 2 []]] --  / \
                                        -- 5    2
isGraceful :: NTree Int -> Bool
isGraceful NEmpty                  = True
isGraceful (NNode parent children) = all (checkNode parent) children && all isGraceful children
    where checkNode parentValue (NNode value _) = even $ abs (parentValue - value)


-- Друго представяне на дървета с произволен брой наследници: Асоциативен списък [(а, [а])], където a е типът на върховете на дървото. 
-- Ключове в списъка са върховете на дървото, а асоциираната с даден ключ стойност е списък от синовете на съответния връх.

-- I. Асоциативен списък, описващ преките наследници (синовете) на върховете, които не са листа.
-- Задача 5. Да се дефинира функция twoChildrenNodes, която намира броя на върховете в дърво, които имат точно два наследника. 
t1 = [(4, [2, 5]), (2, [1, 3])]
{-   4
    / \
   2   5
  / \
 1   3   -}
 
hasTwoChildren :: (Int, [Int]) -> Bool
hasTwoChildren (_, children) = length children == 2

twoChildrenNodes :: [(Int, [Int])] -> Int
twoChildrenNodes [] = 0
twoChildrenNodes (node:rest)
    | hasTwoChildren node = 1 + twoChildrenNodes rest
    | otherwise           = twoChildrenNodes rest

twoChildrenNodes' :: [(Int, [Int])] -> Int
twoChildrenNodes' nodes = sum [if hasTwoChildren node then 1 else 0 | node <- nodes]

twoChildrenNodes'' :: [(Int, [Int])] -> Int
twoChildrenNodes'' nodes = length $ filter hasTwoChildren nodes

-- Задача 6. Да се дефинира функция allHaveTwoChildren, която проверява дали всички върхове (които не са листа) в дървото имат точно по два наследника.
-- 1ви вариант
allHaveTwoChildren :: [(Int, [Int])] -> Bool
allHaveTwoChildren []          = True
allHaveTwoChildren (node:rest) = hasTwoChildren node && allHaveTwoChildren rest
-- 2ри вариант
allHaveTwoChildren' :: [(Int, [Int])] -> Bool
allHaveTwoChildren' nodes = foldr1 (&&) (map hasTwoChildren nodes) -- чрез map-ването получаваме списък от булеви стойности, след което fold-ваме с логическо 'и' - ако всичките са True, резултата на fold-а също ще е True
-- 3ти вариант
allHaveTwoChildren'' :: [(Int, [Int])] -> Bool
allHaveTwoChildren'' nodes = all hasTwoChildren nodes

-- II. Асоциативен списък, описващ преките наследници (синовете) на всички върхове (включително и листата - за тях списъкът от наследници е празен).
-- Задача 7. Дефинирайте функция findUncles, която за дадени дърво tree и връх node на tree намира списък от всички чичовци на node в tree.
t2 = [(1,[2,3,4]),(2,[5,6]),(3,[7]),(4,[8,9]),(5,[]),(6,[10]),(7,[]),(8,[]),(9,[]),(10,[])]
{-     1 
  2    3    4
 5 6   7   8 9
    10         -}

findUncles :: [(Int, [Int])] -> Int -> [Int]
findUncles tree node = if null parent then [] else brothers (head parent)
    where 
        parent = [v | (v, vs) <- tree, elem node vs] -- бащата на node е ключът на двойката, в която node присъства като елемент на списъка от наследници
        brothers v = concat [delete v vs | (_, vs) <- tree, elem v vs] -- тук очакваме всеки връх да е уникален (т.е. да е уникално цяло число), така че v ще се среща само веднъж
        -- delete е вградена функция от Data.List, която премахва първото срещане на даден елемент в списък. Така премахваме бащата, за да получим само братята му.
        -- При list comprehension-а всъщност получаваме списък от списъци (който съдържа само един елемент), затова се налага с concat да го превърнем в обикновен списък.



-- Обратно към двоични дървета.
-- Задача 8. Казваме, че едно двоично дърво е огледално-симетрично, ако лявото му поддърво е огледален образ на дясното.
-- Да се дефинира предикат isSymmetric :: BTree Int -> Bool, който проверява дали дадено двоично дърво е огледално-симетрично.
isSymmetric :: BTree Int -> Bool
isSymmetric Empty               = True
isSymmetric (Node _ left right) = areMirrored left right
    where
        areMirrored :: BTree Int -> BTree Int -> Bool
        areMirrored Empty Empty                     = True
        areMirrored (Node x1 l1 r1) (Node x2 l2 r2) = x1 == x2 && areMirrored l1 r2 && areMirrored r1 l2

t3 :: BTree Int                     --   1
t3 = Node 1 (Node 2 Empty Empty)    --  / \
            (Node 3 Empty Empty)    -- 2   3

t4 :: BTree Int                             --     1
t4 = Node 1 (Node 2 (Node 3 Empty Empty)    --    / \
                    Empty)                  --   2   2
            (Node 2 Empty                   --  /     \
                    (Node 3 Empty Empty))   -- 3       3

t5 :: BTree Int                                     --       1
t5 = Node 1 (Node 2 (Node 3 Empty Empty)            --    /     \
                    (Node 7 (Node 5 Empty Empty)    --   2       2
                            Empty))                 --  / \     / \
            (Node 2 (Node 7 Empty                   -- 3   7   7   3
                            (Node 5 Empty Empty))   --    /     \
                    (Node 3 Empty Empty))           --   5       5


-- Задача 9. Да се дефинира функция containsWord :: BTree Char -> String -> Bool, която по дадено двоично дърво от символи и дума, 
-- съставена от поне една буква, проверява дали думата се среща в дървото, като последният символ от думата е лист в това дърво.
charTree1 :: BTree Char                                --     a
charTree1 = Node 'a' (Node 'c' (Node 'f' Empty Empty)  --    / \
                               (Node 'd' Empty Empty)) --   c   b
                     (Node 'b' Empty                   --  / \   \
                               (Node 'e' Empty Empty)) -- f   d   e

-- грешно решение - позволява думи, които не са съставени от последователни върхове - например "af" в горното дърво
containsWord :: BTree Char -> String -> Bool
containsWord Empty                 _         = False
containsWord (Node x Empty Empty) [c]        = x == c -- имаме листо и последната буква на думата, проверяваме дали съвпадат
containsWord (Node x left right)  str@(c:cs) = if x == c then containsWord left cs || containsWord right cs -- търсим остатъка от думата
                                                         else containsWord left str || containsWord right str -- търсим цялата дума в поддърветата
containsWord _                    []         = False -- изчерпали сме думата преди да стигнем до листо

-- за да избегнем това, първо ще дефинираме фунцкия, която търси думи, започващи от корена на дървото и завършващи с листо
rootWord :: BTree Char -> String -> Bool
rootWord Empty                _      = False
rootWord (Node x Empty Empty) [c]    = x == c
rootWord (Node x left right)  (c:cs) = x == c && (rootWord left cs || rootWord right cs)
rootWord _                    []     = False

-- за да намерим и думите, които не започват от корена, просто ще извикаме горната функция за всички поддървета, за да видим дали поне едно от тях съдържа думата
containsWord' :: BTree Char -> String -> Bool
containsWord' Empty                    _    = False
containsWord' tree@(Node _ left right) word = rootWord tree word || containsWord' left word || containsWord' right word 

-- Задача 10. Да се дефинира функция genWords :: BTree Char -> [String], която по дадено двоично дърво от символи връща списък от всички думи, съдържащи се в него.
genWordsStartingFrom :: BTree Char -> [String]
genWordsStartingFrom Empty                = []
genWordsStartingFrom (Node c Empty Empty) = [[c]]
genWordsStartingFrom (Node c left right)  = [c:cs | cs <- genWordsStartingFrom left ++ genWordsStartingFrom right]

genWords :: BTree Char -> [String]
genWords Empty                  = []
genWords tree@(Node _ left right) = genWordsStartingFrom tree ++ genWords left ++ genWords right

-- Задача 11. Да се дефинира функция allContain :: [BTree Char] -> [String], която по даден списък от двоични дървета от символи 
-- връща списък от тези думи, които се съдържат във всички дървета.
charTree2 :: BTree Char                               --     a
charTree2 = Node 'a' (Node 'c' (Node 'd' Empty Empty) --    / \
                               Empty)                 --   c   b
                     (Node 'b' Empty Empty)           --  /
                                                      -- d      
allContain :: [BTree Char] -> [String]
allContain []     = []
allContain trees  = foldl1 intersect $ map genWords trees -- първо генерираме списъците от думи на всички дървета, и после взимаме сечението на всички тези списъци като fold-нем с вградената функция intersect.


main :: IO()
main = do
    -- print $ getExpression expr1 -- -> "((5+(2*5))-4)"

    -- print $ calc expr2  -- -> 11.0

    -- print $ calc' (Leaf "1")          -- -> 1.0
    -- print $ calc' (Leaf "123.456789") -- -> 123.456789
    -- print $ calc' expr3               -- -> 11.0


    -- print $ maxDepthBlueNode colorTree   -- -> 3
    -- print $ maxDepthNode colorTree Red   -- -> 4
    -- print $ maxDepthNode colorTree Green -- -> 4
    -- print $ maxDepthNode colorTree Blue  -- -> 3
    

    -- print $ size nTree1 -- -> 7
    -- print $ isGraceful nTree2 -- -> True
    -- print $ isGraceful nTree3 -- -> False


    -- print $ twoChildrenNodes [(4, [2, 5]), (2, [1, 3])] -- -> 2 (възлите 4 и 2)
    -- print $ twoChildrenNodes [(10,[3,7,12]),(3,[5,8,9]),(7,[11,13]),(12,[6,4]),(8,[1,2])] -- -> 3 (възлите 7, 12 и 8)
    
    -- print $ allHaveTwoChildren [(10,[3,7,12]),(3,[5,8,9]),(7,[11,13]),(12,[6,4]),(8,[1,2])] -- -> False
    -- print $ allHaveTwoChildren [(4, [2, 5]), (2, [1, 3])] -- -> True
    
    -- print $ findUncles t2 5  -- -> [3,4]
    -- print $ findUncles t2 7  -- -> [2,4]
    -- print $ findUncles t2 10 -- -> [5]
    -- print $ findUncles t2 2  -- -> []


    -- print $ isSymmetric t3 -- -> False
    -- print $ isSymmetric t4 -- -> True
    -- print $ isSymmetric t5 -- -> True


    -- print $ containsWord' charTree1 "acd" -- -> True
    -- print $ containsWord' charTree1 "cd"  -- -> True
    -- print $ containsWord' charTree1 "ac"  -- -> False
    -- print $ containsWord charTree1 "af"   -- -> True, а трябва да е False
    -- print $ containsWord' charTree1 "af"  -- -> False
 
    -- print $ genWords charTree1 -- -> ["acf","acd","abe","cf","cd","f","d","be","e"]
    -- print $ genWords charTree2 -- -> ["acd","ab","cd","d","b"]
 
    -- print $ allContain [charTree1, charTree2] -- -> ["acd","cd","d"]
