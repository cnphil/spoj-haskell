-- TLE
-- going to rewrite using ByteString
nextPalin :: String -> String
nextPalin orig
    | allNines orig     = "1" ++ (take (len - 1) (repeat '0')) ++ "1"
    | len == 1  = show (((read orig) :: Int) + 1)
    | odd len = ((removeIndex (len `div` 2 + 1)) . appendReverse . addOne . (take (len `div` 2 + 1))) orig
    | even len = let origDouble =  (appendReverse . (take (len `div` 2))) orig in
                     if origDouble > orig then
                        origDouble
                     else
                        (appendReverse . addOne . (take (len `div` 2))) orig
    where len = length orig
          addOne str = show ((read str) + 1)
          appendReverse str = str ++ (reverse str)
          removeIndex idx str = fst splits ++ ((tail . snd) splits)
                    where splits = splitAt (idx - 1) str

allNines = all (== '9')

main :: IO ()
main = do 
    cases <- readLn :: IO Int
    sequence_ (fmap (>>= putStrLn) (take cases (repeat (fmap nextPalin getLine))))
