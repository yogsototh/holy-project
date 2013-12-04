{-# LANGUAGE DeriveDataTypeable #-}
module HolyProject where
import HolyProject.GitConfig        ( getNameAndMailFromGitConfig)
import HolyProject.StringUtils      ( projectNameFromString
                                    , capitalize
                                    , checkProjectName)
import HolyProject.GithubAPI        ( searchGHUser)
import HolyProject.MontyPython      ( bk
                                    , you
                                    , ask
                                    )

-- Get current year for the License
import Data.Time.Clock
import Data.Time.Calendar
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
-- Fork
import Control.Concurrent
-- Get external file of package
import Paths_holy_project

import Data.Maybe                   (fromMaybe)
import Control.Monad                (void)

-- | Record containing all information to initialize a project
data Project = Project {
      projectName :: String
    , moduleName :: String
    , author :: String
    , mail :: String
    , ghaccount :: String
    , synopsis :: String
    , year :: String } deriving (Data, Typeable)

-- | Randomly choose an end scenario and then show a \"serious\" error message
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

-- | Ask, questions and create the initial project
holyStarter :: IO ()
holyStarter = do
    intro
    (name,email) <- getNameAndMailFromGitConfig
    earlyhint <- newEmptyMVar
    maybe   (putMVar earlyhint Nothing) -- if no email found put Nothing
            (\hintmail ->               -- in the other case ask the github API
                void (forkIO (putMVar earlyhint =<< searchGHUser hintmail)))
            email
    project <- ask "project name" Nothing
    ioassert (checkProjectName project)
             "Use only letters, numbers, spaces ans dashes please"
    let projectname = projectNameFromString project
        modulename  = capitalize project
    in_author       <- ask "name" name
    in_email        <- ask "email" email
    ghUserHint      <- if fromMaybe "" email /= in_email
                            then searchGHUser in_email
                            else takeMVar earlyhint
    in_ghaccount    <- ask "github account" ghUserHint
    in_synopsis     <- ask "project in less than a dozen word?" Nothing
    current_year    <- getCurrentYear
    createProject $ Project projectname modulename in_author in_email
                            in_ghaccount in_synopsis current_year
    end

-- | Simply return the current year as String
getCurrentYear :: IO String
getCurrentYear = do
    -- (current_year,_,_) <- getCurrentTime >>= return . toGregorian . utctDay
    (current_year,_,_) <- fmap (toGregorian . utctDay) getCurrentTime
    return (show current_year)

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
    ioassert (not dirExists) (projectName p ++ " directory already exists")
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
        , ( "interact"
          , "interact"
          )
        , ( "project.cabal"
          , projectName p ++ ".cabal"
          )
        , ( "src/Main.hs"
          , "src"  </> "Main.hs"
          )
        , ( "src/ModuleName.hs"
          , "src"  </> (moduleName p++".hs")
          )
        , ( "src/ModuleName/Coconut.hs"
          , "src"  </> moduleName p </> "Coconut.hs"
          )
        , ( "src/ModuleName/Swallow.hs"
          , "src"  </> moduleName p </> "Swallow.hs"
          )
        , ( "test/ModuleName/Coconut/Test.hs"
          , "test" </> moduleName p </> "Coconut" </> "Test.hs"
          )
        , ( "test/ModuleName/Swallow/Test.hs"
          , "test" </> moduleName p </> "Swallow" </> "Test.hs"
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
    _ <- system $ "./.cabal-sandbox/bin/test-" ++ projectName p
    return ()
