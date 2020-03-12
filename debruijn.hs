indexOf :: (Eq a, Num t) => a -> [a] -> t
indexOf element alphabet = indexOfHelper element alphabet 0
    where indexOfHelper element alphabet i = if element == head alphabet then i else indexOfHelper element (tail alphabet) (i + 1)


update :: Eq a => [a] -> [a] -> [a]
update alphabet str = 
    if str == [] then
        []
    else if last (str) == last (alphabet) then
        update alphabet (init str)
    else 
        init str ++ [alphabet!!(indexOf (last str) alphabet + 1)]


nWord :: Int -> [a] -> [a]
nWord n str = 
    if length str < n then 
        nWord n (str ++ str)
    else
        take n str


algorithm :: Eq a => Int -> [a] -> [a] -> [a]
algorithm n alphabet sequence =
    if sequence == [] then
        []
    else if  n `mod` length sequence == 0 then
        sequence ++ algorithm n alphabet (update alphabet (nWord n sequence))
    else 
        algorithm n alphabet (update alphabet (nWord n sequence))


generator :: (Eq a, Num a, Enum a) => Int -> a -> [a]
generator n k = algorithm n [0..(k - 1)] [0] 


convert :: Show a => [a] -> [Char]
convert list = (map (show) list) >>= id


deBruijn :: (Show a, Eq a, Num a, Enum a) => Int -> a -> FilePath -> IO ()
deBruijn n k fileName = 
    writeFile fileName (convert (generator n k ))

-- Ciąg de Bruijna. Napisać program, który generuje ciąg de Bruijna rzędu n nad
-- alfabetem k-elementowym. Liczby te powinny być pierwszym i drugim argumentem 
-- wywołania programu; trzeci argument to nazwa pliku, do którego należy
-- zapisać wynik. Teoria:
-- https://en.wikipedia.org/wiki/De_Bruijn_sequence,
-- http://mathworld.wolfram.com/deBruijnSequence.html

-- przykład:
-- deBruijn 2 2 "result.txt"