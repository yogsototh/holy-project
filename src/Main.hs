module Main where

import System.Console.ANSI

output :: ConsoleIntensity -> ColorIntensity -> Color -> String -> IO ()
output bold intensity color str = do
    setSGR  [ SetColor Foreground intensity color
            , SetConsoleIntensity bold
            ]
    putStr str
    setSGR []


bk :: String -> IO ()
bk str = output NormalIntensity Dull Green ("Bridgekeeper: " ++ str ++ "\n")
bkn :: String -> IO ()
bkn str = output NormalIntensity Dull Green ("Bridgekeeper: " ++ str)
you :: String -> IO ()
you str = output NormalIntensity Dull Yellow ("Bridgekeeper: " ++ str ++ "\n")

intro :: IO ()
intro = do
    bk "Stop!"
    bk "Who would cross the Bridge of Death"
    bk "must answer me these questions three,"
    bk "ere the other side he see."
    you "Ask me the questions, bridgekeeper, I am not afraid.\n"

main :: IO ()
main = do
    intro
