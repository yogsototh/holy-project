module HolyProject.StringUtils.Test
( stringUtilsSuite
) where
import              Test.Tasty                      (testGroup, TestTree)
import              Test.Tasty.SmallCheck           (forAll)
import qualified    Test.Tasty.SmallCheck       as  SC
import qualified    Test.Tasty.QuickCheck       as  QC
import              Test.SmallCheck.Series          (Serial)
import              HolyProject.StringUtils

stringUtilsSuite :: TestTree
stringUtilsSuite = testGroup "StringUtils"
    [ SC.testProperty "SC projectNameFromString idempotent" $
            idempotent projectNameFromString
    , SC.testProperty "SC capitalize idempotent" $
            deeperIdempotent capitalize
    , QC.testProperty "QC projectNameFromString idempotent" $
            idempotent capitalize
    ]

idempotent f = \s -> f s == f (f s)

deeperIdempotent :: (Eq a, Show a, Serial m a) => (a -> a) -> SC.Property m
deeperIdempotent f = forAll $ SC.changeDepth1 (+1) $ \s -> f s == f (f s)

