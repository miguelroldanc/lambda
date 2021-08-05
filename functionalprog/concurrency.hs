-- Hello World with many threads
import System.Process
import Control.Concurrent
import System.Exit

main = do
        let cmds = replicate 100 "sleep 10s"
        ps <- mapM runCommand cmds
        es <- mapM waitForProcess ps
        case length . filter (/= ExitSuccess) $ es of
            0 -> putStrLn "All commands ran successfully."
            n -> putStrLn ("There were " ++ show n ++ " failures.")