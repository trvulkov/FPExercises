import Data.Char(isLetter, isDigit, toUpper)
import Data.List(isPrefixOf, tails, elemIndex, nub, minimumBy)
import Data.Maybe(fromJust)
import Data.Function(on)

-- Задача 1. Нормализация на входните данни - Енигма, както повечето криптиращи машини от това време, е разполагала с клавиатура със само 26-те главни букви от латинската азбука.
-- Затова, преди да бъдат криптирани, всички съобщения трябвало да бъдат приведени в т. нар. нормален вид: всички числени стойности бивали изписвани словом, всички малки букви ставали главни,
-- а интервалите и пунктуационните знакове били премахвани или заменяни с кодови комбинации от главни букви (напр. интервалът бил заменян с X и т. н.).

-- Напишете функция normalize message, която нормализира входното съобщение. Правилата за нормализация са следните: 
-- - всички малки букви стават главни
-- - ако съобщението съдържа цифри, функцията връща грешка
-- - всички останали знакове се игнорират
normalize :: String -> String
normalize [] = []
normalize (x:xs)
    | isDigit x  = error "digits not allowed"
    | isLetter x = toUpper x : normalize xs
    | otherwise  = normalize xs

normalize' :: String -> String
normalize' msg = if any isDigit msg then error "digits not allowed" else map toUpper $ filter isLetter msg

-- Задача 2. Цезаров шифър - Цезаровият шифър е един от най-простите и най-стари методи за криптиране на съобщения. Първото му известно използване е от Юлий Цезар по време на кампаниите му в Галия,
-- откъдето идва и неговото име. Идеята на Цезаровия шифър е проста: вземаме съобщението, което искаме да шифроваме, и заместваме всяка от буквите в него с буквата, отместена с определен брой позиции в азбуката.
-- Например, ако отместването е 3, то тогава ‘A’ -> ‘D’, ‘B’ -> ‘E’, ‘C’ -> ‘F,’ ... , ‘X’ -> ‘A’, ‘Y’ -> ‘B’, ‘Z’ -> ‘C’.

-- а) Напишете функция encode alphabet ch offset, която приема списък от знакове alphabet, знак ch и отместване offset и връща знака от alphabet, отместен на offset от ch (по модул дължината на списъкa).
-- Функцията encode трябва да работи както с положително, така и с отрицателно отместване и да връща грешка, ако ch не е елемент на alphabet.
-- N.B. Не е задължително буквите в alphabet да са подредени от ‘A’ до ‘Z’, т.е. НЕ може да разчитате на функциите ord и chr!
encode :: String -> Char -> Int -> Char
encode alphabet ch offset = if notElem ch alphabet then error ("unsupported symbol: " ++ [ch]) else alphabet!!position
    where position = (fromJust (elemIndex ch alphabet) + offset) `mod` (length alphabet)
    -- elemIndex връща стойност от специалния тип Maybe, който се ползва за представяне на опционални стойности, т.е. стойности които може да липсват. Например конкретно за тази функция - търсим индекса на
    -- даден елемент в списък, но ако той не присъства в списъка, няма как да върнем валидна стойност. Има различни начини за справяне с такива проблеми - един от най-простите, с който може би сте се сблъсквали
    -- в С/С++, е просто да връщаме -1 или други подобни "невалидни" стойности, но той има доста недостатъци. По-добрите начини са exception-и, или ползването на специални типове както тук.
    -- Типът Maybe е алгебричен тип с два конструктора - Nothing (представя липсата на стойност) и Just a (опакова стойност от тип 'а'). За да достъпим самата стойност в Just конструктора (все едно да я
    -- разопаковаме) трябва да ползваме функцията fromJust. elemIndex връща Nothing ако елемента не присъства в списъка в който търсим, и (Just индекса) ако присъства, но тъй като ние викаме position само за
    -- такива букви, които вече сме проверили че са елементи (тъй като не са изпълнили notElem, not(notElem) === elem), така че може спокойно да извикаме fromJust.
 
-- с типа Maybe може да се справим и чрез pattern match-ване:
encode' :: String -> Char -> Int -> Char
encode' alphabet ch offset = helper (elemIndex ch alphabet)
    where
        helper Nothing      = error ("unsupported symbol: " ++ [ch])
        helper (Just index) = alphabet !! ((index + offset) `mod` (length alphabet))

-- б) Напишете функция encrypt alphabet offset normalized, която приема азбука alphabet, отместване offset и съобщение в нормализиран вид и връща съобщението, 
-- криптирано със съответното отместване.
encrypt :: String -> Int -> String -> String
encrypt alphabet offset normalized = [encode alphabet letter offset | letter <- normalized]

-- в) Напишете функция decrypt alphabet offset encrypted, която приема отместване offset и съобщение, криптирано с това отместване, и връща оригиналното съобщение в нормализиран вид.
-- Можете да използвате факта, че декриптирането на Цезаров шифър с дадено отместване offset е еквивалентно на криптиране с отместване -offset.
decrypt :: String -> Int -> String -> String
decrypt alphabet offset encrypted = encrypt alphabet (-offset) encrypted

-- Задача 3. Атака на Цезаровия шифър - Една от основните слабости на Цезаровия шифър се състои в това, че броят на възможните шифри е ограничен до броя на ротациите на буквите в азбуката минус едно. 
-- Това прави Цезаровия шифър податлив на т. нар. brute force атака, т.е. атака, която генерира всички възможни дешифровки на кодираното съобщение.

-- а) Напишете функцията crackAll alphabet encrypted, която връща списък от всички възможни дешифровки на кодираното съобщение encrypted.
crackAll :: String -> String -> [String]
crackAll alphabet encrypted = [decrypt alphabet offset encrypted | offset <- [1 .. (length alphabet - 1)]]
 
-- б) След като сме генерирали всички възможни дешифровки, бихме могли лесно да намерим най-вероятните от тях, използвайки факта, че някои кратки думи, например the, at, on, се срещат много често в
-- английския език. За тази цел най-напред напишете функция substring sub str, която проверява дали поднизът sub се среща в низа str.
substring :: String -> String -> Bool
substring sub str = any (isPrefixOf sub) (tails str)

-- в) Използвайте функциите от предишните две подточки, за да напишете функцията crackCandidates alphabet commonWords encrypted, която приема списък с често срещани думи и криптирано съобщение и
-- връща списък с потенциални вероятни разшифровки. Една разшифровка се смята за вероятна, ако съдържа поне една от думите от списъка с често срещани думи.
crackCandidates :: String -> [String] -> String -> [String]
crackCandidates alphabet commonWords encrypted = [x | x <- crackAll alphabet encrypted, isCandidate x]
    where isCandidate str = any (`substring` str) commonWords
 
-- Задача 4. Polysubstitution cypher (шифър с множествено заместване) - Един от простите начини да се справим със слабостта на Цезаровия шифър е да разбием съобщението на блокове от по няколко знака и
-- да криптираме всеки от тях с различен Цезаров шифър, отместен с определена стъпка спрямо предишния.

-- а) Напишете функция polyEncrypt alphabet offset step blockSize normalized, която приема азбука alphabet, първоначално отместване offset, стъпка step и размер на блока blockSize,
-- както и съобщение в нормализиран вид normalized, и връща криптирано съобщение, първите blockSize знака на което се криптират с отместване offset, следващите blockSize знака – с отместване offset+step и т.н. 
polyEncrypt :: String -> Int -> Int -> Int -> String -> String
polyEncrypt _        _      _    _         ""         = ""
polyEncrypt alphabet offset step blockSize normalized = encrypt alphabet offset (take blockSize normalized) ++ 
                                                        polyEncrypt alphabet (offset + step) step blockSize (drop blockSize normalized)

-- б) Напишете функция polyDecrypt alphabet offset step blockSize encrypted, която декриптира съобщението от предишната подточка. 
polyDecrypt :: String -> Int -> Int -> Int -> String -> String
polyDecrypt alphabet offset step blockSize encrypted = polyEncrypt alphabet (-offset) (-step) blockSize encrypted

-- Задача 5. Емулация на Eнигма - Един от основните компоненти на Енигма е система от ротори, всеки от които може да се моделира като polysubstitution cypher от предната задача. 
-- Резултатът от всеки от роторите се предава като вход на следващия. Резултатът от последния ротор е криптираното съобщение.

-- а) Напишете функция enigmaEncrypt alphabet rotors normalized, която приема азбука alphabet, списък от ротори (offset, step, blockSize) и съобщение в нормализиран вид normalized и
-- връща криптираното от роторите съобщение. 
enigmaEncrypt :: String -> [(Int,Int,Int)] -> String -> String
enigmaEncrypt _        []                               message    = message
enigmaEncrypt alphabet ((offset,step,blockSize):rotors) normalized = enigmaEncrypt alphabet rotors (polyEncrypt alphabet offset step blockSize normalized)

-- б) Напишете функция enigmaDecrypt alphabet rotors normalized, която приема азбука, списък от ротори и криптирано съобщение и връща оригиналното съобщение. 
enigmaDecrypt :: String -> [(Int,Int,Int)] -> String -> String
enigmaDecrypt _        []                               message    = message
enigmaDecrypt alphabet ((offset,step,blockSize):rotors) normalized = enigmaDecrypt alphabet rotors (polyDecrypt alphabet offset step blockSize normalized)



-- Задача 7. Да се дефинира функция transpose :: [[a]] -> [[a]], която получава матрица, представена като списък от списъци и я транспонира.
transpose :: [[a]] -> [[a]]
transpose []     = []
transpose ([]:_) = [] -- трябва да разгледаме този случай, за да си подсигурим че разглеждания ред е непразен, тъй като в следващия случай взимаме head
transpose xss    = map head xss : transpose (map tail xss) -- Взимаме 1ви стълб, като вземем главите на всички редове. След това продължаваме с остатъка от матрицата - 
                                                           -- махаме първия ѝ стълб, като вземем опашките на всичките редове.
                                                           
-- Задача 8. Да се дефинира функция rotate :: [[a]] -> [[a]], която завърта матрица на прав ъгъл по обратна на часовниковата стрелка посока.
rotate :: [[a]] -> [[a]]
rotate = reverse . transpose

-- Задача 9. Да се дефинира функция spiral :: [[a]] -> [a], която обхожда матрица по спирала, започвайки от първия елемент на първия ред и движейки се отвън навътре по посока на часовника до 
-- изчерпване на членовете (т.е. 1ви ред, последна колона, последен ред наобратно, първа колона наобратно, втори ред, предпоследна колона, ...).
spiral :: [[a]] -> [a]
spiral []       = []
spiral (xs:xss) = xs ++ spiral (rotate xss)

mat :: [[Int]]
mat = [[1,2,3],
       [4,5,6],
       [7,8,9],
       [10,11,12]]    

-- Задача 10. Нека са дефинирани следните типове:
type Student = String                    -- име на ученик
type Subject = String                    -- име на предмет
type Grade   = Double                    -- оценка
type Record  = (Student, Subject, Grade) -- запис за ученик, съдържащ име на ученик, учебен предмет и оценката на ученика по дадения предмет.
-- Дефинирайте функцията hardestSubject :: [Record] -> Subject, която получава списък от записи за учениците от даден клас и връща името на предмета с най-ниска средна оценка за този клас.
hardestSubject :: [Record] -> Subject
hardestSubject rs = fst $ minimumBy (compare `on` snd) averagesList -- или foldr1 (\ r1@(_, avg1) r2@(_, avg2) -> if avg1 < avg2 then r1 else r2) averagesList -- 
    where 
        getNotes forSubject = [note | (_, subject, note) <- rs, subject == forSubject]

        averagesList = [(subject, average (getNotes subject)) | subject <- nub [subject | (_, subject, _) <- rs ]]
        average xs = sum xs / fromIntegral (length xs)

r1,r2,r3,r4,r5,r6 :: Record
r1 = ("Ivan",   "Algebra",  5.5)
r2 = ("Georgi", "Algebra",  4.0)
r3 = ("Atanas", "Algebra",  4.25)
r4 = ("Atanas", "Algebra",  6.0)
r5 = ("Ivan",   "Geometry", 5.0)
r6 = ("Georgi", "Geometry", 5.0)
r7 = ("Atanas", "Geometry", 5.0)

rs :: [Record]
rs = [r1,r2,r3,r4,r5,r6,r7]

-- Задача 11. Да се дефинира функция calcLuhnChecksum n, която приема целочислен аргумент n и пресмята неговата чексума на Лун. Алгоритъмът на Лун се състои от следните стъпки:
-- 1. Конструрира се списък lst от цифрите на даденото цяло число (списък от едноцифрените цели числа, записани чрез поредните цифри в десетичния запис на числото).
-- 2. Обхождат се елементите на lst и тези на четна позиция се умножават по 2 (индексирането в случая започва от 1).
-- 3. Събират се цифрите на числата, получени на предходната стъпка.
-- 4. Получената сума се умножава по 9. Резултатът е последната цифра на полученото число.
-- Повече информация за алгоритъма на Лун, за неговите свойства и приложения може да се намери тук: https://en.wikipedia.org/wiki/Luhn_algorithm.
-- Пример: Нека разгледаме числото 7992739871.
-- 1. Цифри                                             -> 7 9  9 2 7 3 9 8  7 1
-- 2. Удвояване на едноцифрените числа на четни позиции -> 7 18 9 4 7 6 9 16 7 2
-- 3. Събиране на цифрите                               -> 7 9  9 4 7 6 9 7  7 2
-- 4. Сумата на числата в клетките от третия ред е 67. 67 * 9 = 603, следователно търсената чексума е 3.
calcLuhnChecksum :: Int -> Int
calcLuhnChecksum n = (9 * finalSum) `mod` 10
    where
        digitList x = if x < 10 then [x] else digitList (x `div` 10) ++ [x `mod` 10]
        
        doubleEven []     _ = []
        doubleEven (x:xs) i = if even i then 2*x : doubleEven xs (i+1) else x : doubleEven xs (i+1)

        digitSum x = sum $ digitList x
        finalSum = sum $ map digitSum $ doubleEven (digitList n) 1


main :: IO()
main = do
    -- print $ normalize "Attack London tommorow at ten a.m." -- -> "ATTACKLONDONTOMMOROWATTENAM"
    -- print $ normalize "Attack London tommorow at 10 a.m."  -- error: ... digits not allowed
    -- print $ normalize' "Attack London tommorow at ten a.m." -- -> "ATTACKLONDONTOMMOROWATTENAM"
    -- print $ normalize' "Attack London tommorow at 10 a.m."  -- error: ... digits not allowed

    -- print $ encode ['A'..'Z'] 'A' 1     -- -> 'B'
    -- print $ encode ['A'..'Z'] 'C' 2     -- -> 'E'
    -- print $ encode ['A'..'Z'] 'Z' 3     -- -> 'C'
    -- print $ encode ['A'..'Z'] 'A' (-1)  -- -> 'Z'
    -- print $ encode ['A'..'Z'] 'C' (-2)  -- -> 'A'
    -- print $ encode ['A'..'Z'] 'Z' (-3)  -- -> 'W'
    -- print $ encode ['A'..'Z'] '@' 1     -- error: ... unsupported symbol: @
    -- print $ encode ['A'..'Z'] 'a' 1     -- error: ... unsupported symbol: a
    -- print $ encode ['A','C'..'Z'] 'C' 3 -- -> 'I' (азбуката ни всъщност е A,C,E,G,I,..., така че 3 символа след C е I)
 
    -- print $ encrypt ['A'..'Z'] 5 "ATTACKLONDONTOMORROWATTENAM" -- -> "FYYFHPQTSITSYTRTWWTBFYYJSFR"
    -- print $ decrypt ['A'..'Z'] 5 "FYYFHPQTSITSYTRTWWTBFYYJSFR" -- -> "ATTACKLONDONTOMORROWATTENAM"

    -- print $ crackAll ['A'..'Z'] "FYYFHPQTSITSYTRTWWTBFYYJSFR" 
    {- -> ["EXXEGOPSRHSRXSQSVVSAEXXIREQ","DWWDFNORQGRQWRPRUURZDWWHQDP","CVVCEMNQPFQPVQOQTTQYCVVGPCO","BUUBDLMPOEPOUPNPSSPXBUUFOBN","ATTACKLONDONTOMORROWATTENAM",
    "ZSSZBJKNMCNMSNLNQQNVZSSDMZL","YRRYAIJMLBMLRMKMPPMUYRRCLYK","XQQXZHILKALKQLJLOOLTXQQBKXJ","WPPWYGHKJZKJPKIKNNKSWPPAJWI","VOOVXFGJIYJIOJHJMMJRVOOZIVH",
    "UNNUWEFIHXIHNIGILLIQUNNYHUG","TMMTVDEHGWHGMHFHKKHPTMMXGTF","SLLSUCDGFVGFLGEGJJGOSLLWFSE","RKKRTBCFEUFEKFDFIIFNRKKVERD","QJJQSABEDTEDJECEHHEMQJJUDQC",
    "PIIPRZADCSDCIDBDGGDLPIITCPB","OHHOQYZCBRCBHCACFFCKOHHSBOA","NGGNPXYBAQBAGBZBEEBJNGGRANZ","MFFMOWXAZPAZFAYADDAIMFFQZMY","LEELNVWZYOZYEZXZCCZHLEEPYLX",
    "KDDKMUVYXNYXDYWYBBYGKDDOXKW","JCCJLTUXWMXWCXVXAAXFJCCNWJV","IBBIKSTWVLWVBWUWZZWEIBBMVIU","HAAHJRSVUKVUAVTVYYVDHAALUHT","GZZGIQRUTJUTZUSUXXUCGZZKTGS"] -}
    -- print $ substring "Haskell" "Haskell Curry" -- -> True
    -- print $ substring "Curry" "Haskell Curry"   -- -> True
    -- print $ substring "Turing" "Haskell Curry"  -- -> False
    -- print $ crackCandidates ['A'..'Z'] ["THE","AND","AT","ON","IS"] "FYYFHPQTSITSYTRTWWTBFYYJSFR" -- -> ["ATTACKLONDONTOMORROWATTENAM"]
 
    -- print $ polyEncrypt ['A'..'Z'] 5 1 7 "ATTACKLONDONTOMORROWATTENAM" -- -> "FYYFHPQUTJUTZUTVYYVDHBBMVIU"
    -- print $ polyDecrypt ['A'..'Z'] 5 1 7 "FYYFHPQUTJUTZUTVYYVDHBBMVIU" -- -> "ATTACKLONDONTOMORROWATTENAM"
    -- print $ enigmaEncrypt ['A'..'Z'] [(5,1,1),(7,2,10),(13,3,25)] "ATTACKLONDONTOMORROWATTENAM" -- -> "ZTUCFOQUULZZGCBEIJHQXRSEOFS"
    -- print $ enigmaDecrypt ['A'..'Z'] [(5,1,1),(7,2,10),(13,3,25)] "ZTUCFOQUULZZGCBEIJHQXRSEOFS" -- -> "ATTACKLONDONTOMORROWATTENAM"


    -- print $ transpose mat   -- -> [[1,4,7,10],
    --                         --   [2,5,8,11],
    --                         --   [3,6,9,12]]
                    
    -- print $ rotate mat      -- -> [[3,6,9,12],
    --                         --   [2,5,8,11],
    --                         --   [1,4,7,10]]
                 
    -- print $ spiral mat      -- -> [1,2,3,6,9,12,11,10,7,4,5,8]

    -- print $ hardestSubject rs  -- -> "Algebra"

    -- print $ calcLuhnChecksum 7992739871 -- -> 3
    -- print $ calcLuhnChecksum 343577563  -- -> 2
    -- print $ calcLuhnChecksum 645652345  -- -> 3
    -- print $ calcLuhnChecksum 3453       -- -> 8
