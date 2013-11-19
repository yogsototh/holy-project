{-# LANGUAGE DeriveDataTypeable #-}
module Main where

-- Project name manipulation
import Data.Char            (toUpper,toLower,isLetter,isNumber)
import Data.List            (intersperse)
import Data.List.Split
-- Console read write with colors
import System.Console.ANSI
import System.IO            (hFlush, stdout)
-- Hastache
import Control.Applicative
import Data.Data
import Text.Hastache
import Text.Hastache.Context
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as LZ
import System.Directory

-- Get external file of package
import Paths_holy_project

-- | Record containing all information to initialize a project
data Project = Project {
      projectName :: String
    , moduleName :: String
    , author :: Maybe String
    , mail :: Maybe String
    , ghaccount :: Maybe String
    , synopsis :: Maybe String } deriving (Data, Typeable)

ioassert :: Bool -> String -> IO ()
ioassert True _ = return ()
ioassert False str = error str

-- | Ask, questions and create the initial project
main :: IO ()
main = do
    intro
    project <- ask "project name"
    ioassert (checkProjectName project)
             "Use only letters, numbers, spaces ans dashes please"
    let projectname = projectNameFromString project
        modulename = capitalize project
    putStrLn $ "Project: " ++ projectname
    putStrLn $ "Module: " ++ modulename
    author <- ask "name"
    email <- ask "email"
    ghaccount <- ask "github account"
    synopsis <- ask "project in less than a dozen word?"
    createProject $ Project projectname modulename
                        (toJust author) (toJust email)
                        (toJust ghaccount) (toJust synopsis)
    end
    where
        toJust [] = Nothing
        toJust str = Just str

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
ask :: String -> IO String
ask info = do
    bk $ "What is your " ++ info ++ "?"
    putStr "> "
    hFlush stdout
    answer <- getLine
    putStrLn ""
    return answer

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


genFile :: MuContext IO -> [Char] -> [Char] -> IO ()
genFile context filename outputFileName = do
    putStrLn $ '\t':outputFileName
    pkgfileName <- getDataFileName ("scaffold/"++filename)
    template <- BS.readFile pkgfileName
    transformedFile <- hastacheStr defaultConfig template context
    LZ.writeFile outputFileName transformedFile

createProject :: Project -> IO ()
createProject p = do
    let context = mkGenericContext p
    createDirectory (projectName p)
    setCurrentDirectory (projectName p)
    putStrLn "I'm not a witch, I'm not a witch!"
    genFile context "gitignore"                         $ ".gitignore"
    genFile context "LICENSE"                           $ "LICENSE"
    genFile context "Setup.hs"                          $ "Setup.hs"
    genFile context "project.cabal"                     $ (projectName p) ++ ".cabal"
    genFile context "src/Main.hs"                       $ "src/Main.hs"
    genFile context "src/ModuleName.hs"                 $ "src/"++(moduleName p)++".hs"
    genFile context "src/ModuleName/Coconut.hs"         $ "src/"++(moduleName p)++"/Coconut.hs"
    genFile context "src/ModuleName/Swallow.hs"         $ "src/"++(moduleName p)++"/Swallow.hs"
    genFile context "test/ModuleName/Coconut/Test.hs"   $ "test/"++(moduleName p)++"/Coconut/Test.hs"
    genFile context "test/ModuleName/Swallow/Test.hs"   $ "test/"++(moduleName p)++"/Swallow/Test.hs"
    genFile context "test/Test.hs"                      $ "test/Test.hs"
