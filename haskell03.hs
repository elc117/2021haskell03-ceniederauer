-- Prática 03 de Haskell
-- Nome: Carlos Eduardo Niederauer Rodrigues

add10toall :: [Int] -> [Int]
add10toall lis = [x + 10 | x <- lis]

multN :: Int -> [Int] -> [Int]
multN n lis = [x * n | x <- lis] 

multN' :: Int -> [Int] -> [Int]
multN' n lis = map (\x -> x*n) lis

applyExpr :: [Int] -> [Int]
applyExpr lis = [3*x+2 | x <- lis]

addSuffix :: String -> [String] -> [String]
addSuffix s str = [x ++ s | x <- str]

selectgt5 :: [Int] -> [Int]
selectgt5 lis = [x | x <- lis, x > 5]

sumOdds :: [Int] -> Int
sumOdds i = sum [x | x <- i, odd x]

sumOdds' :: [Int] -> Int
sumOdds' n = sum ( filter (\x->x `mod` 2 == 1) n)

selectExpr :: [Int] -> [Int]
selectExpr n = [x | x <- n, even x, x > 20 && x < 50]

countShorts :: [String] -> Int
countShorts