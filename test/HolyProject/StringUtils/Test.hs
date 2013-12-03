module HolyProject.StringUtils.Test
( stringUtilsSuite
) where
import              Data.Char                       (isControl,isSymbol)
import              Test.Tasty                      (testGroup, TestTree)
import              Test.Tasty.HUnit
import              Test.Tasty.SmallCheck           (forAll)
import qualified    Test.Tasty.SmallCheck       as  SC
import qualified    Test.Tasty.QuickCheck       as  QC
import              Test.SmallCheck.Series          (Serial)
import              HolyProject.StringUtils

tp name prop = testGroup name
    [ QC.testProperty "QC" prop
    , SC.testProperty "SC" prop
    ]

stringUtilsSuite :: TestTree
stringUtilsSuite = testGroup "StringUtils"
    [ tp "projectNameFromString idempotent" $
         idempotent projectNameFromString
    , SC.testProperty "capitalize idempotent" $
         deeperIdempotent capitalize
    , tp "no space in project name" $
         \s -> dropWhile (/=' ') (projectNameFromString s) == []
    , tp "no space in capitalized name" $
         \s -> dropWhile (/=' ') (capitalize s) == []
    , tp "no dash in capitalized name" $
            \s -> dropWhile (/='-') (capitalize s) == []
    , tp "no control char" $
        \s -> if (s /= "") && (checkProjectName s == True) then (all (not . isControl) s) else True
    , tp "no symbol char" $
        \s -> if (s /= "") && (checkProjectName s == True) then (all (not . isSymbol) s) else True
    , testCase "doesn't accept empty project name" $
            checkProjectName "" @=? False
    ]

idempotent :: (Eq a) => (a -> a) -> a -> Bool
idempotent f = \s -> f s == f (f s)

deeperIdempotent :: (Eq a, Show a, Serial m a) => (a -> a) -> SC.Property m
deeperIdempotent f = forAll $ SC.changeDepth1 (+1) $ \s -> f s == f (f s)

