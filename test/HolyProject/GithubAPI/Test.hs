module HolyProject.GithubAPI.Test
( githubAPISuite
) where
import Test.Tasty (testGroup, TestTree)
import Test.Tasty.HUnit
import HolyProject.GithubAPI

githubAPISuite :: TestTree
githubAPISuite = testGroup "GithubAPI"
    [ testCase "Yann"
        (ioTestEq
            (searchGHUserFromEmail "Yann.Esposito@gmail.com")
            (Just "\"yogsototh\""))
    , testCase "Jasper"
        (ioTestEq
            (searchGHUserFromEmail "Jasper Van der Jeugt")
            (Just "\"jaspervdj\""))
        ]

ioTestEq :: (Eq a, Show a) => IO a -> a -> Assertion
ioTestEq action expected = action >>= assertEqual "" expected

