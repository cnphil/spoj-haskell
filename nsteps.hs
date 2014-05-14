diag :: Int -> Int
diag n
    | even n = 2 * n
    | otherwise = 1 + diag (n - 1)

getNumber :: (Int, Int) -> String
getNumber (x, y)
    | x < 0 || y < 0 = "No Number"
    | x == y         = show $ diag x
    | x - 2 == y     = show $ diag x - 2
    | otherwise      = "No Number"

main :: IO ()
main = interact g
    where g :: String -> String
          g = unlines . (map $ getNumber . parseNumber) . tail . lines

parseNumber :: String -> (Int, Int)
parseNumber = wrapTuple . (map read) . words
            where wrapTuple :: [Int] -> (Int, Int)
                  wrapTuple (a:b:_) = (a,b)
