{-# LANGUAGE DeriveDataTypeable #-}
module Main where

-- Project name manipulation
import Data.Char                    (toUpper,toLower,isLetter,isNumber)
import Data.List                    (intersperse)
import Data.List.Split
-- Get current year for the License
import Data.Time.Clock
import Data.Time.Calendar
-- Console read write with colors
import System.Console.ANSI
import System.IO                    (hFlush, stdout)
-- Hastache
import Data.Data
import Text.Hastache
import Text.Hastache.Context
-- File and directory Handling
import qualified Data.ByteString            as  BS
import qualified Data.ByteString.Lazy.Char8 as  LZ
import System.Directory
import System.FilePath.Posix        (takeDirectory,(</>))
-- Execute external commands
import System.Cmd                   (system)
-- Random error message :)
import System.Random
--- Environment variable
import System.Environment           (getEnv)
import Data.Maybe                   (fromJust)
import Control.Exception
import System.IO.Error
import Control.Monad                (guard)

-- Get external file of package
import Paths_holy_project

-- | Record containing all information to initialize a project
data Project = Project {
      projectName :: String
    , moduleName :: String
    , author :: String
    , mail :: String
    , ghaccount :: String
    , synopsis :: String
    , year :: String } deriving (Data, Typeable)

holyError :: String -> IO ()
holyError str = do
    r <- randomIO
    if r
        then
            do
                bk "What... is your favourite colour?"
                you "Blue. No, yel..."
                putStrLn "[You are thrown over the edge into the volcano]"
                you "You: Auuuuuuuuuuuugh"
                bk " Hee hee heh."
        else
            do
                bk "What is the capital of Assyria?"
                you "I don't know that!"
                putStrLn "[You are thrown over the edge into the volcano]"
                you "Auuuuuuuuuuuugh"
    error ('\n':str)

ioassert :: Bool -> String -> IO ()
ioassert True _ = return ()
ioassert False str = holyError str

safeReadGitConfig :: IO LZ.ByteString
safeReadGitConfig = do
    e <- tryJust (guard . isDoesNotExistError)
                 (do
                    home <- getEnv "HOME"
                    LZ.readFile $ home ++ "/.gitconfig" )
    return $ either (const (LZ.empty)) id e

-- | Ask, questions and create the initial project
main :: IO ()
main = do
    intro
    gitconfig <- safeReadGitConfig
    let (name,email) = getNameAndMail gitconfig
    project <- ask "project name" Nothing
    ioassert (checkProjectName project)
             "Use only letters, numbers, spaces ans dashes please"
    let projectname = projectNameFromString project
        modulename  = capitalize project
    in_author       <- ask "name" name
    in_email        <- ask "email" email
    in_ghaccount    <- ask "github account" Nothing
    in_synopsis     <- ask "project in less than a dozen word?" Nothing
    current_year    <- getCurrentYear
    createProject $ Project projectname modulename in_author in_email
                            in_ghaccount in_synopsis current_year
    end

getCurrentYear :: IO String
getCurrentYear = do
    (current_year,_,_) <- getCurrentTime >>= return . toGregorian . utctDay
    return (show current_year)

-- | bridgekeeper speak
bk :: String -> IO ()
bk str = colorPutStr Green ("Bridgekeeper: " ++ str ++ "\n")
-- | bridgekeeper speak without line return
bkn :: String -> IO ()
bkn str = colorPutStr Green ("Bridgekeeper: " ++ str)
-- | the user dialog
you :: String -> IO ()
you str = colorPutStr Yellow ("Bridgekeeper: " ++ str ++ "\n")

-- | show color
colorPutStr :: Color -> String -> IO ()
colorPutStr color str = do
    setSGR  [ SetColor Foreground Dull color
            , SetConsoleIntensity NormalIntensity
            ]
    putStr str
    setSGR []


-- | Show an introduction test
intro :: IO ()
intro = do
    bk "Stop!"
    bk "Who would cross the Bridge of Death"
    bk "must answer me these questions three,"
    bk "ere the other side he see."
    you "Ask me the questions, bridgekeeper, I am not afraid.\n"

-- | Show the final dialog
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

-- | Ask for some info and returns it
ask :: String -> Maybe String -> IO String
ask info hint = do
    bk $ "What is your " ++ info ++ "?" ++ (maybe "" (\h -> " ("++h++")") hint)
    putStr "> "
    hFlush stdout
    answer <- getLine
    putStrLn ""
    return $ if (answer == "") && (hint /= Nothing) then fromJust hint else answer

-- | verify if project is conform
checkProjectName :: String -> Bool
checkProjectName [] = False
checkProjectName str = all (\c -> (isLetter c)||(isNumber c)||(c=='-')||(c==' ')) str

-- | transform a chain like "Holy project" in "holy-project"
projectNameFromString :: String -> String
projectNameFromString str = concat $ intersperse "-" (splitOneOf " -" (map toLower str))

-- | transform a chain like "Holy project" in "HolyProject"
capitalize :: String -> String
capitalize str = concat (map capitalizeWord (splitOneOf " -" str))
    where
        capitalizeWord :: String -> String
        capitalizeWord (x:xs)   = (toUpper x):map toLower xs
        capitalizeWord  _       = []


genFile :: MuContext IO -> FilePath -> FilePath -> IO ()
genFile context filename outputFileName = do
    putStrLn $ '\t':outputFileName
    pkgfileName <- getDataFileName ("scaffold/" ++ filename)
    template <- BS.readFile pkgfileName
    transformedFile <- hastacheStr defaultConfig template context
    createDirectoryIfMissing True (takeDirectory outputFileName)
    LZ.writeFile outputFileName transformedFile

createProject :: Project -> IO ()
createProject p = do
    let context = mkGenericContext p
    dirExists <- doesDirectoryExist (projectName p)
    ioassert (not dirExists) ((projectName p) ++ " directory already exists")
    createDirectory (projectName p)
    setCurrentDirectory (projectName p)
    genFile context "gitignore"                       $ ".gitignore"
    genFile context "auto-update"                     $ "auto-update"
    genFile context "LICENSE"                         $ "LICENSE"
    genFile context "Setup.hs"                        $ "Setup.hs"
    genFile context "project.cabal"                   $ (projectName p) ++ ".cabal"
    genFile context "src/Main.hs"                     $ "src"  </> "Main.hs"
    genFile context "src/ModuleName.hs"               $ "src"  </> ((moduleName p)++".hs")
    genFile context "src/ModuleName/Coconut.hs"       $ "src"  </> (moduleName p) </> "Coconut.hs"
    genFile context "src/ModuleName/Swallow.hs"       $ "src"  </> (moduleName p) </> "Swallow.hs"
    genFile context "test/ModuleName/Coconut/Test.hs" $ "test" </> (moduleName p) </> "Coconut" </> "Test.hs"
    genFile context "test/ModuleName/Swallow/Test.hs" $ "test" </> (moduleName p) </> "Swallow" </> "Test.hs"
    genFile context "test/Test.hs"                    $ "test" </> "Test.hs"
    _ <- system "git init ."
    _ <- system "cabal sandbox init"
    _ <- system "cabal install"
    _ <- system "cabal test"
    _ <- system $ "./.cabal-sandbox/bin/test-" ++ (projectName p)
    return ()

getNameAndMail :: LZ.ByteString -> (Maybe String,Maybe String)
getNameAndMail gitConfigContent = (selectElem "name",selectElem "email")
    where
        conflines :: [[LZ.ByteString]]
        conflines = map LZ.words (LZ.lines gitConfigContent)

        selectElem :: String -> Maybe String
        selectElem elm = msafeHead $
                            filter (/= Nothing)
                                   (map (getElem elm) conflines)

        msafeHead :: [Maybe a] -> Maybe a
        msafeHead [] = Nothing
        msafeHead (x:_) = x

        getElem :: String -> [LZ.ByteString] -> Maybe String
        getElem el (n:e:xs) = if (n == (LZ.pack el)) && (e == (LZ.pack "="))
                                then Just (LZ.unpack (LZ.unwords xs))
                                else Nothing
        getElem _ _ = Nothing
