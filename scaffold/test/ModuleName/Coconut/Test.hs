{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module {{moduleName}}.Coconut.Test
    (coconutSuite)
where

import Test.Tasty (testGroup, TestTree)
import Test.Tasty.HUnit
import Test.Tasty.SmallCheck as SC

import {{moduleName}}.Coconut

-- Make instance of CoconutDataStruct
-- we simply use consN Constr where N is the arity of Constr (SmallCheck)
-- we also needed the
-- {-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
import Test.SmallCheck.Series
instance Monad m => Serial m CoconutDataStruct  where series = cons1 CoconutConstr
-- Now we could test properties with smallcheck on CoconutDataStruct type.

coconutSuite :: TestTree
coconutSuite = testGroup "coconut"
    [ testCase "coconut" testCoconut
    , SC.testProperty "coconut property" prop_coconut
    ]

testCoconut :: Assertion
testCoconut = coconut @=? 10

prop_coconut :: Property IO
prop_coconut = forAll $ \coconutStruct -> coconutfunc coconutStruct >= 0
