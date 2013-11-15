module Main where

import Test.Tasty (defaultMain,testGroup,TestTree)

import HolyProject.Swallow.Test
import HolyProject.Coconut.Test

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "All Tests"
            [ swallowSuite
            , coconutSuite
            ]
