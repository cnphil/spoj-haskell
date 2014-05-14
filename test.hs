main :: IO ()

main = do
    answer <- fmap (unlines . fst . (break (=="42")) . lines) getContents
    putStr answer


