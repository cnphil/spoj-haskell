import Control.Monad

main :: IO ()
main = do
    n <- readLn :: IO Int
    when (n /= 0) (do
        putStrLn $ show $ feynman n
        main)

feynman :: Int -> Int
feynman n
    | n == 0 = 0
    | n == 1 = 1
    | otherwise = 2 * sum [n-1,n-2..1] + n + feynman (n - 1) 
