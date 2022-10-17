import Data.List(group)
import Data.Char(digitToInt, isLetter, isDigit)

-- Run-length encoding е прост начин за компресия на текст, при който последователните срещания на един елемент (символ от текста) се заменят с <брой на срещания><елемент>, а в случай,
-- че в резултат на тази замяна биха се получили поне толкова символи, колкото се съдържат в оригиналния текст, се запазва оригиналният текст.
-- Например, ако имаме само едно срещане на буквата "а" и го заменим с "1а", то промененият текст ще има повече символи от оригиналния.

-- Задача 1. Да се дефинира функция encode :: String -> String, която компресира низ по описания метод.
encode :: String -> String
encode cs = concat $ map transform (group cs)
    where transform cs = if length cs <= 2 then cs else show (length cs) ++ [head cs] -- show е вградена функция, която конвертира аргумента си до низ. В този случай, ще конвертираме число до низ от цифрите му.

-- Задача 2. Да се дефинира обратната функция decode :: String -> String, която от низ, компресиран по описания метод, възстановява оригиналния низ.
decode :: String -> String
decode ""  = ""
decode [c] = [c]
decode str = if number > 0 then replicate number c ++ decode cs else c : decode cs
    where
        (num, c:cs) = break isLetter str -- break ще върне двойка от списъци: първият ще съдържа всички елементи до първият който изпълнява предиката (в този случай - до първият който е буква), 
                                         -- а вторият - всички останали елементи. Така num ще съдържа броя последователни срещания (ако е бил по-голям от 2), c ще е буквата, а cs ще е остатъка от низа.
        digits = map digitToInt num -- digitToInt е вградена функция от Data.Char, която конвертира символ за цифра до съответното цяло число. С map на тази функция върху низ от цифри ще получим списък от
                                    -- цифрите на числото, но вече представени като цели числа.
        number = if null digits then 0 else foldl1 ((+) . (*10)) digits -- с този fold от списък от цифри ще получим самото число - например за [1,2,3] ще получим (1*10 + 2)*10 + 3
        -- number = if null num then 0 else read num -- може да стане и с read - така от низ с цифри директно ще получим числото като Int

decode' :: String -> String
decode' cs = helper cs 0
    where 
        helper "" _ = ""
        helper (c:cs) k
            | isDigit c = helper cs (k * 10 + digitToInt c)
            | k == 0    = c : helper cs 0
            | otherwise = replicate k c ++ helper cs 0

main :: IO()
main = do
    print $ encode "Haskell"         -- -> "Haskell"
    print $ encode "aaabccdefff"     -- -> "3abccde3f"
    print $ encode "aaaaaaaaaaaabbb" -- -> "12a3b"
    print $ encode "aabbb"           -- -> "aa3b"

    print $ decode "12a3b" -- -> "aaaaaaaaaaaabbb"
    print $ decode "a3b"   -- -> "abbb"
    print $ decode "aa3b"  -- -> "aabbb"
