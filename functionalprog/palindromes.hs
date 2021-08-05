import System.IO
main = do
    withFile "test2.txt" ReadMode (\handle -> do
        contents <- hGetContents handle
        putStr $ respondePalindromes contents)

respondePalindromes = unlines . map (\xs ->
    if isPalindrome xs then "palindrome" else "not a palindrome") . lines
    where isPalindrome xs = xs == reverse xs