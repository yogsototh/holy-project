module Main where

import System.Console.ANSI
import System.IO            (hFlush, stdout)

colorPutStr :: Color -> String -> IO ()
colorPutStr color str = do
    setSGR  [ SetColor Foreground Dull color
            , SetConsoleIntensity NormalIntensity
            ]
    putStr str
    setSGR []


bk :: String -> IO ()
bk str = colorPutStr Green ("Bridgekeeper: " ++ str ++ "\n")
bkn :: String -> IO ()
bkn str = colorPutStr Green ("Bridgekeeper: " ++ str)
you :: String -> IO ()
you str = colorPutStr Yellow ("Bridgekeeper: " ++ str ++ "\n")

intro :: IO ()
intro = do
    bk "Stop!"
    bk "Who would cross the Bridge of Death"
    bk "must answer me these questions three,"
    bk "ere the other side he see."
    you "Ask me the questions, bridgekeeper, I am not afraid.\n"

end :: IO ()
end = do
    putStrLn "\n\n"
    bk "What... is the air-speed velocity of an unladen swallow?"
    you "What do you mean? An African or European swallow?"
    bk "Huh? I... I don't know that."
    putStrLn "[the bridgekeeper is thrown over]"
    bk "Auuuuuuuuuuuugh"
    putStrLn "Sir Bedevere: How do you know so much about swallows?"
    you "Well, you have to know these things when you're a king, you know."

ask :: String -> IO String
ask info = do
    bk $ "What is your " ++ info ++ "?"
    putStr "> "
    hFlush stdout
    answer <- getLine
    putStrLn ""
    return answer

main :: IO ()
main = do
    intro
    _ <- ask "project name"
    _ <- ask "name"
    _ <- ask "email"
    _ <- ask "github account"
    _ <- ask "project in less than a dozen word?"
    end
