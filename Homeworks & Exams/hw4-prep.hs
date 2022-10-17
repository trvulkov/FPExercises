-- Задача 1. Нека е дефиниран тип “сметка” Account, който представя вектор с три елемента (Int,Int,Double) - идентификатор на сметка, идентификатор на човек и баланс по сметката.
type Account = (Int, Int, Double)
-- Нека е дефиниран и тип “човек” Person, който представя вектор с три елемента (Int,String,String) - идентификатор на човек, име и местоживеене (населено място).
type Person = (Int, String, String)
-- Ще работим с "база от данни" на банка, представена като двойка от списък от сметки и списък от хора.
-- а) Дефинирайте функция getAverageBalance :: ([Account],[Person]) -> (Person -> Bool) -> Double, която получава като аргументи база от данни и предикат p.
-- Функцията трябва да връща средния баланс по всички сметки на хората, които удовлетворяват предиката p.
getAverageBalance :: ([Account],[Person]) -> (Person -> Bool) -> Double
getAverageBalance (accounts, people) p = sum matchingBalances / fromIntegral (length matchingBalances)
    where
        matchingIDs      = [personID | person@(personID, _, _) <- people, p person]
        matchingBalances = [balance | (_, personID, balance) <- accounts, elem personID matchingIDs]

-- б) Дефинирайте на функционално ниво, чрез getAverageBalance, функция averageBalanceOfCities :: ([Account],[Person]) -> [String] -> Double, която получава база от данни и списък от населени места и
-- връща като резултат средния баланс по сметките на хората с местоживеене в някое от изброените в списъка населени места.
averageBalanceOfCities :: ([Account], [Person]) -> [String] -> Double
averageBalanceOfCities database cities = getAverageBalance database (\ (_,_,city) -> elem city cities)

people1, people2 :: [Person]
people1 = [(1, "Ivan", "Sofia"), (2, "Georgi", "Burgas"), (3, "Petar", "Plovdiv"), (4, "Petya", "Burgas")]
people2 = [(97, "Ivan", "Sofia"), (98, "Georgi", "Varna"), (87, "Igor", "Sofia")]

accounts1, accounts2 :: [Account]
accounts1 = [(1, 1, 12.5), (2, 1, 123.2), (3, 2, 13.0), (4, 2, 50.2), (5, 2, 17.2), (6, 3, 18.3), (7, 4, 19.4)]
accounts2 = [(1, 97, 350.0), (2, 98, 400.0), (3, 87, 200.0)]

-- Задача 2. Нека е дефинирано следното представяне на двоично дърво:
data BTree = Empty | Node Int BTree BTree
-- Ще наричаме един възел в дървото "интересен", ако неговата стойност е равна на 2^k, където k е броят на преките му наследници.
-- Дефинирайте функция countInteresting :: BTree -> Int, намираща броя на "интересните" възли в дадено двоично дърво.
countInteresting :: BTree -> Int
countInteresting Empty = 0
countInteresting tr@(Node x left right) = if x == 2^(descendantCount tr) then 1 + countInteresting left + countInteresting right else countInteresting left + countInteresting right
    where 
        descendantCount Empty                              = 0
        descendantCount (Node _ Empty        Empty)        = 0
        descendantCount (Node _ (Node _ _ _) Empty)        = 1
        descendantCount (Node _ Empty        (Node _ _ _)) = 1
        descendantCount (Node _ (Node _ _ _) (Node _ _ _)) = 2

t1 :: BTree                               --    16
t1 = Node 16 (Node 0 Empty Empty)         --   /  \
            (Node 4 (Node 1 Empty Empty)  --  0    4
                    (Node 0 Empty Empty)) --      / \
                                          --     1   0

t2 :: BTree                               --    4
t2 = Node 4 (Node 0 Empty Empty)          --   / \
            (Node 2 (Node 1 Empty Empty)  --  0   2
                    Empty)                --     /
                                          --    1                               



main :: IO()
main = do
    print $ getAverageBalance (accounts1, people1) (\ (_,_,city) -> city == "Burgas")      -- -> 24.950000000000003
    print $ getAverageBalance (accounts1, people1) (\ (_,(n:_),_) -> n == 'P')             -- -> 18.85
    print $ getAverageBalance (accounts2, people2) (\ (id,_,_) -> id > 90)                 -- -> 375.0 = (350+400)/2
    print $ averageBalanceOfCities (accounts1, people1) ["Sofia","Gabrovo","Stara Zagora"] -- -> 67.85
    print $ averageBalanceOfCities (accounts1, people1) ["Sofia","Gabrovo","Burgas"]       -- -> 39.24999999999999
    print $ averageBalanceOfCities (accounts2, people2) ["Sofia","Gabrovo","Stara Zagora"] -- -> 275.0 = (350+200)/2

    print $ countInteresting t1 -- -> 2 (4=2^2,  1=2^0)
    print $ countInteresting t2 -- -> 3 (4=2^2,  2=2^1, 1=2^0)
