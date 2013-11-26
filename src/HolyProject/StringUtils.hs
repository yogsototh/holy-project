module HolyProject.StringUtils where

-- Project name manipulation
import Data.Char                    (toUpper,toLower,isLetter,isNumber)
import Data.List                    (intersperse)
import Data.List.Split              (splitOneOf)

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

-- | verify if a project name is conform
checkProjectName :: String -> Bool
checkProjectName [] = False
checkProjectName str = all (\c -> (isLetter c)||(isNumber c)||(c=='-')||(c==' ')) str

