import Control.Monad

main = do
    colors <- forM [1,2,3,4] (\a -> do
        putStrLn ( "Tell me the color number " ++ show a)
        color <- getLine
        return color)
    putStrLn "The colors with their numbers are: "
    mapM putStrLn colors