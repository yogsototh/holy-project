module HolyProject.GitConfig
( getNameAndMailFromGitConfig
)
where

import qualified    Data.ByteString.Lazy.Char8 as   LZ
import              Control.Monad                   (guard)
import              Control.Exception
import              System.IO.Error
import              System.Environment              (getEnv)

-- | Return name and email in gitconfig if found
getNameAndMailFromGitConfig :: IO (Maybe String, Maybe String)
getNameAndMailFromGitConfig = return . getNameAndMail =<< safeReadGitConfig

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

-- | Get the first line which start with
-- 'elem =' and return the third field (value)
getFirstValueFor :: [[LZ.ByteString]] -> String -> Maybe String
getFirstValueFor splitted keyname = firstJust (map (getValueForKey keyname) splitted)

-- | return the first Just value of a list of Maybe
firstJust :: (Eq a) => [Maybe a] -> Maybe a
firstJust l = case dropWhile (==Nothing) l of
    [] -> Nothing
    (j:_) -> j

-- | Given a line of words ("word1":"word2":rest)
-- getValue will return rest if word1 == key
-- 'elem =' or Nothing otherwise
getValueForKey :: String            -- key
                  -> [LZ.ByteString] -- line of words
                  -> Maybe String    -- the value if found
getValueForKey el (n:e:xs) = if (n == (LZ.pack el)) && (e == (LZ.pack "="))
                        then Just (LZ.unpack (LZ.unwords xs))
                        else Nothing
getValueForKey _ _ = Nothing

