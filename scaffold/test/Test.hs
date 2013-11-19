module Main where

import Test.Tasty (defaultMain,testGroup,TestTree)

import {{moduleName}}.Swallow.Test
import {{moduleName}}.Coconut.Test

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "All Tests"
            [ swallowSuite
            , coconutSuite
            ]
