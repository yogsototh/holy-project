module HolyProject.StringUtils.Test
( stringUtilsSuite
) where
import Test.Tasty (testGroup, TestTree)
import Test.Tasty.HUnit
import HolyProject.StringUtils

stringUtilsSuite :: TestTree
stringUtilsSuite = testGroup "StringUtils"
    [ testCase "projectNameFromString space"
        (testProjectNameFromString "Holy Project" "holy-project")
    , testCase "projectNameFromString dash"
        (testProjectNameFromString "Holy-Project" "holy-project")
    , testCase "projectNameFromString caps"
        (testProjectNameFromString "Holy PROJECT" "holy-project")
    , testCase "projectNameFromString underscore"
        (testProjectNameFromString "Holy_PROJECT" "holy_project")
    ]

testProjectNameFromString :: String -> String -> Assertion
testProjectNameFromString input expectedoutput =
    expectedoutput @=? projectNameFromString input
