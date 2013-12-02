module HolyProject.StringUtils.Test
( stringUtilsSuite
) where
import Test.Tasty (testGroup, TestTree)
import Test.Tasty.HUnit
import HolyProject.StringUtils

stringUtilsSuite :: TestTree
stringUtilsSuite = testGroup "StringUtils"
    [ testGroup "projectNameFromString HUnit"
        $ map (testEq projectNameFromString)
            [ ("space","Holy Project","holy-project")
            , ("empty","","")
            , ("number","12345","12345")
            ]
    ]

testEq :: (Eq a, Show a) =>
            (t -> a)        -- ^ Function to test
            -> (String,t,a) -- ^ (name,input,expected output)
            -> TestTree
testEq f (name,input,expected) = testCase name (f input @?= expected)
