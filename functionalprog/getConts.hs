import System.IO
main = do
    withFile "test1.txt" ReadMode (\handle -> do
        contents <- getContents
        putStr  contents)

shortLinesOnly :: String -> String
shortLinesOnly input = 
    let allLines = lines input
        shortLines = filter (\line -> length line < 10) allLines
        result = unlines shortLines
    in result