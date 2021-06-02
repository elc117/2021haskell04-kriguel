-- Prática 04 de Haskell
-- Nome: Leonardo Cargnin Krugel

import Text.Printf


-- 1)
faixaIdoso :: Int -> String
faixaIdoso idade
    | idade <= 64 && idade >=60 = "IDO64"
    | idade <= 69 && idade >= 65 = "IDO69"
    | idade <= 74 && idade >= 70 = "IDO74"
    | idade <= 79 && idade >= 75 = "IDO79"
    | idade >= 80 = "IDO80"
    | otherwise = "ND"


-- 2)
classifIdosos :: [(String,Int)] -> [(String,Int,String)]
classifIdosos lista = zip3 [fst x | x <- lista] [snd y | y <- lista] [faixaIdoso (snd y) | y <- lista]


-- 3) 
classifIdosos' :: [(String,Int)] -> [(String,Int,String)]
classifIdosos' lista = zip3 (map fst lista) (map snd lista) (map faixaIdoso (map snd lista))


-- 4)
strColor :: (Int,Int,Int) -> String
strColor (r,g,b) = printf "rgb(%d,%d,%d)" r g b


-- 5)
genCircs :: Int -> (Int,Int) -> Int -> [(Int,Int,Int)]
--genCircs n coord r = take n [(fst coord, snd coord, r), ((fst coord) + 2, snd coord, r)..]
genCircs n (cx,cy) r = take n [(x,cy,r) | x <- (iterate (+2) cx)]


-- 6) ATENCAO: se forem requisitadas muitas cores diferentes ela vai ficar presa em uma execucao infinita.
genReds :: Int -> [(Int,Int,Int)]
genReds n = take n [(x,0,0) | x <- [2,2 + truncate (fromIntegral n*1.33)..], x < 255]

