module HolyProject.MontyPython
( bk
, you
, ask
)
where
-- Console read write with colors
import System.Console.ANSI
import System.IO                    (hFlush, stdout)
import Data.Maybe                   (fromJust,isJust)

-- | bridgekeeper speak
bk :: String -> IO ()
bk str = colorPutStr Green ("Bridgekeeper: " ++ str ++ "\n")
-- | the user dialog
you :: String -> IO ()
you str = colorPutStr Yellow ("Sir Yourself: " ++ str ++ "\n")

-- | show color
colorPutStr :: Color -> String -> IO ()
colorPutStr color str = do
    setSGR  [ SetColor Foreground Dull color
            , SetConsoleIntensity NormalIntensity
            ]
    putStr str
    setSGR []

-- | Ask for some info and returns it
ask ::  String          -- ^ What? \"name\" for example
        -> Maybe String -- ^ Default value
        -> IO String
ask info hint = do
    bk $ "What is your " ++ info ++ "?" ++
         maybe "" (\h -> " ("++h++")") hint
    putStr "> "
    hFlush stdout
    answer <- getLine
    putStrLn ""
    return $ if (answer == "") && isJust hint
                then fromJust hint
                else answer

