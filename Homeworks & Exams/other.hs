; Задача 10. Да се дефинира функция (filtered-sum p? f), която приема предикат p? и едноаргументна функция f и връща едноаргументна функция.
; Върнатата от filtered-sum функция трябва да приема като аргумент списък от цели положителни числа xs.
; Нека S е сумата на всички числа от xs – такива, че стойността на предиката p? за квадрата на индекса им в xs е истина.
; Върнатата функция трябва да върне резултата от прилагането на f към сумата S.
(define (filtered-sum p? f)
  (define (get-sum xs i)
    (cond [(null? xs)   0]
          [(p? (* i i)) (+ (car xs) (get-sum (cdr xs) (+ i 1)))]
          [else         (get-sum (cdr xs) (+ i 1))]))
  (λ (xs) (f (get-sum xs 0))))
;((filtered-sum (λ (z) (= 0 (quotient z 3)))  (λ (s) (* s 2))) '(1 2 3 4)) ; -> 6 (за индексите 0 и 1 е изпълнено условието, сумата на съответните елементи е 1 + 3 = 3, и f 3 = 3 * 2 = 6)
;((filtered-sum (λ (z) (= 0 (remainder z 3))) (λ (s) (* s 2))) '(1 2 3 4)) ; -> 10 (за индексите 0 и 3 е изпълнено условието, сумата  на съответните елементи е 1 + 4 = 5, и f 5 = 5 * 2 = 10)
;((filtered-sum (λ (z) (> z 3))               (λ (s) (* s 2))) '(1 2 3 4)) ; -> 14 (за индексите 2 и 3 е изпълнено условието, сумата  на съответните елементи е 3 + 4 = 7, и f 7 = 7 * 2 = 14)




-- Задача 1. Ако са дадени две едноаргументни функции над цели числа f1 и f2, да се дефинира функция на Haskell split f1 f2, 
-- която да връща като резултат едноаргументна функция над цели числа, която приема стойностите на f1 при аргументи, кратни на 3, 
-- и стойностите на f2 при аргументи, които не са кратни на 3.
split :: (Int -> Int) -> (Int -> Int) -> (Int -> Int)
split f1 f2 = \ x -> if x `mod` 3 == 0 then f1 x else f2 x
 --print $ split (\ x -> x) (\ x -> x + 1) 1 -- -> 2
 --print $ split (\ x -> x) (\ x -> x + 1) 3 -- -> 3





-- Задача 1. (Racket или Haskell). Да се дефинира функция graphContainsPoints, която приема списък от наредени двойки от вида (x, y) за Haskell 
-- или точкови двойки (x . y) за Racket, където x и y и са координатите на точка в равнината, и едноаргументна числова функция f. 
-- Функцията да връща истина, ако всички точки от подадения списък принадлежат на графиката на функцията и лъжа в противен случай.
graphContainsPoints :: (Eq a, Num a) => [(a, a)] -> (a -> a) -> Bool
graphContainsPoints points f = all (\ (x, fx) -> fx == f x) points
{- Racket:
(define (graphContainsPoints points f)
  (if (empty? points)
    #t
    (and (= (f (caar points)) (cdar points))
         (graphContainsPoints (cdr points) f)))) -}
 --print $ graphContainsPoints [(x, x+1) | x <- [1..10]] (\x -> x + 1) -- -> True
 --print $ graphContainsPoints [(1, 2), (1, 3)] (\x -> x + 1)          -- -> False
 --print $ graphContainsPoints [(1, 2), (2, 3), (3, 4)] (\x -> x + 1)  -- -> True
 --print $ graphContainsPoints [(1, 2), (2, 4), (3, 4)] (\x -> x + 1)  -- -> False

-- Задача 4 (Racket или Haskell). Дефинирайте двуаргументна функция findEndNodes tree node, която получава асоциативен списък tree и цяло положително число node. 
-- Асоциативният списък tree съдържа представянето на насочен граф като списък на съседи. 
-- Ключът на елементите на tree съответства на номер на даден връх на графа, а асоциираната с ключа стойност е списък с преките наследници на дадения връх. 
-- Функцията findEndNodes трябва да връща като резултат списък с всички достижими върхове от връх с номер node (преки и непреки наследници), 
-- които нямат преки наследници.
findEndNodes :: [(Int, [Int])] -> Int -> [Int]
findEndNodes nodes node = nub (helper (adj node))
 where adj n = concat [xs | (x,xs) <- nodes, x == n]
       endNode x = null (adj x)

       helper [] = []
       helper xs = ends ++ helper (concat (map adj rest))
        where (ends, rest) = partition endNode xs
 --print $ findEndNodes [(1,[2,3,4]), (2,[4,5]), (3,[]), (4,[]), (5,[])] 1 -- -> [3,4,5]


-- Fn that accepts a collection of integers and a two argument fn and returns a fn that upon receiving a collection of the same length would apply it to both
applyToInts :: [Int] -> (Int -> Int -> Int) -> ([Int] -> [Int])
applyToInts xs fn = (\ ys -> map (\ t -> (fn (fst t) (snd t))) (zip xs ys))
-- Adjacent nodes in a graph
nodesInGraph :: [(Int, Int)] -> [Int]
nodesInGraph graph = nub $ foldr (\ (x,y) l -> x:y:l) [] graph
adjList :: [(Int, Int)] -> [(Int, [Int])]
adjList graph = map (\node -> (node, foldr (\(lead, neigh) res -> if lead == node then neigh:res else res) [] graph)) (nodesInGraph graph)
