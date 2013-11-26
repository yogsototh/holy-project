{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}
module Main where

-- Project name manipulation
import Data.Char                    (toUpper,toLower,isLetter,isNumber)
import Data.List                    (intersperse)
import Data.List.Split              (splitOneOf)
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
-- HTTP request and JSON handling
import Network.HTTP.Conduit
import Control.Lens.Operators       ((^?))
import Control.Lens.Aeson
import Data.Aeson.Encode            (fromValue)
import qualified Data.Text.Lazy as TLZ
import qualified Data.Text.Lazy.Builder as TLB

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

-- | Error message
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

-- | Assert something true. In any other case show the holy error
ioassert :: Bool -> String -> IO ()
ioassert True _ = return ()
ioassert False str = holyError str

-- | return the content of ~/.gitconfig if it exists
-- if the HOME environment variable is not set
-- or the file doesn't exists
-- We return an empty string
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
    ghUserHint      <- getGHUser in_email
    in_ghaccount    <- ask "github account" ghUserHint
    in_synopsis     <- ask "project in less than a dozen word?" Nothing
    current_year    <- getCurrentYear
    createProject $ Project projectname modulename in_author in_email
                            in_ghaccount in_synopsis current_year
    end

-- | Simply return the current year as String
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
ask ::  String          -- ^ What? "name" for example
        -> Maybe String -- ^ Default value
        -> IO String
ask info hint = do
    bk $ "What is your " ++ info ++ "?" ++
         (maybe "" (\h -> " ("++h++")") hint)
    putStr "> "
    hFlush stdout
    answer <- getLine
    putStrLn ""
    return $ if (answer == "") && (hint /= Nothing)
                then fromJust hint
                else answer

-- | verify if a project name is conform
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

-- | This function use a Data file mustache template
-- and a hastache context to write a destination file
genFile :: MuContext IO -- ^ hastache context
            -> String   -- ^ Data file name (without 'scaffold/' see in .cabal)
            -> FilePath -- ^ The destination file path
            -> IO ()
genFile context filename outputFileName = do
    putStrLn $ '\t':outputFileName  -- show the file name
    template <- BS.readFile =<< getDataFileName ("scaffold/" ++ filename)
    transformedFile <- hastacheStr defaultConfig template context
    createDirectoryIfMissing True (takeDirectory outputFileName)
    LZ.writeFile outputFileName transformedFile

-- | This function is where we create the project once the
-- question are answered
createProject :: Project -> IO ()
createProject p = do
    -- create the hastache context object from the Project data type
    let context = mkGenericContext p
    -- Check if the directory doesn't already exists
    dirExists <- doesDirectoryExist (projectName p)
    ioassert (not dirExists) ((projectName p) ++ " directory already exists")
    -- Create the directory and go into it
    createDirectory (projectName p)
    setCurrentDirectory (projectName p)
    -- Generate all files using data files
    mapM_ (uncurry (genFile context))
        [ ( "gitignore"
          , ".gitignore"
          )
        , ( "auto-update"
          , "auto-update"
          )
        , ( "LICENSE"
          , "LICENSE"
          )
        , ( "Setup.hs"
          , "Setup.hs"
          )
        , ( "project.cabal"
          , (projectName p) ++ ".cabal"
          )
        , ( "src/Main.hs"
          , "src"  </> "Main.hs"
          )
        , ( "src/ModuleName.hs"
          , "src"  </> ((moduleName p)++".hs")
          )
        , ( "src/ModuleName/Coconut.hs"
          , "src"  </> (moduleName p) </> "Coconut.hs"
          )
        , ( "src/ModuleName/Swallow.hs"
          , "src"  </> (moduleName p) </> "Swallow.hs"
          )
        , ( "test/ModuleName/Coconut/Test.hs"
          , "test" </> (moduleName p) </> "Coconut" </> "Test.hs"
          )
        , ( "test/ModuleName/Swallow/Test.hs"
          , "test" </> (moduleName p) </> "Swallow" </> "Test.hs"
          )
        , ( "test/Test.hs"
          , "test" </> "Test.hs"
          )
        ]
    -- Execute some commands
    -- We don't really need them to be succesful
    -- So we try them anyway
    _ <- system "git init ."
    _ <- system "cabal sandbox init"
    _ <- system "cabal install"
    _ <- system "cabal test"
    _ <- system $ "./.cabal-sandbox/bin/test-" ++ (projectName p)
    return ()


-- | Returns the name and email from the content of a .gitconfig file
-- almost equivalent to the two zsh lines:
--
-- > name="$(< ~/.gitconfig awk '$1 == name {shift 2; print}')"
-- > email="$(< ~/.gitconfig awk '$1 == email {shift 2; print}')"
--
-- But in Haskell it doesn't read the entire file.
-- The script after the first occurence of name and email.
getNameAndMail :: LZ.ByteString -> (Maybe String,Maybe String)
getNameAndMail gitConfigContent = (getFirstValueFor splitted "name",
                                   getFirstValueFor splitted "email")
    where
        -- make lines of words
        splitted :: [[LZ.ByteString]]
        splitted = map LZ.words (LZ.lines gitConfigContent)

-- Get the first line which start with
-- 'elem =' and return the third field (value)
getFirstValueFor :: [[LZ.ByteString]] -> String -> Maybe String
getFirstValueFor splitted keyname = firstJust (map (getValueForKey keyname) splitted)

-- return the first Just value of a list of Maybe
firstJust :: (Eq a) => [Maybe a] -> Maybe a
firstJust l = case dropWhile (==Nothing) l of
    [] -> Nothing
    (j:_) -> j

-- Given a line of words ("word1":"word2":rest)
-- getValue will return rest if word1 == key
-- 'elem =' or Nothing otherwise
getValueForKey :: String            -- key
                  -> [LZ.ByteString] -- line of words
                  -> Maybe String    -- the value if found
getValueForKey el (n:e:xs) = if (n == (LZ.pack el)) && (e == (LZ.pack "="))
                        then Just (LZ.unpack (LZ.unwords xs))
                        else Nothing
getValueForKey _ _ = Nothing

simpleHTTPWithUserAgent :: String -> IO LZ.ByteString
simpleHTTPWithUserAgent url = do
    r  <- parseUrl url
    let request = r { requestHeaders =  [ ("User-Agent","HTTP-Conduit") ] }
    body <- withManager $ \manager -> do
                response <- httpLbs request manager
                return $ responseBody response
    return body


-- Ask the github API
-- A strange behaviour you HAVE TO add a User-Agent in your header.
-- It took me way too long to get this error
getGHUser :: String -> IO (Maybe String)
getGHUser email = do
    url = "https://api.github.com/search/users?q=" ++ email
    body <- simpleHTTPWithUserAgent url
    login <- return $ body ^? key "items" . nth 0 . key "login"
    return $ fmap jsonValueToString login
    where
        jsonValueToString = TLZ.unpack . TLB.toLazyText . fromValue
