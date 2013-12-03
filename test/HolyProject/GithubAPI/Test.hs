module HolyProject.GithubAPI.Test
( githubAPISuite
) where
import Test.Tasty (testGroup, TestTree)
import Test.Tasty.HUnit
import HolyProject.GithubAPI

githubAPISuite :: TestTree
githubAPISuite = testGroup "GithubAPI"
    [ testCase "Yann" $ ioTestEq
            (searchGHUser "Yann.Esposito@gmail.com")
            (Just "\"yogsototh\"")
    , testCase "Jasper" $ ioTestEq
            (searchGHUser "Jasper Van der Jeugt")
            (Just "\"jaspervdj\"")
    ]

-- | Test if some IO action returns some expected value
ioTestEq :: (Eq a, Show a) => IO a -> a -> Assertion
ioTestEq action expected = action >>= assertEqual "" expected

