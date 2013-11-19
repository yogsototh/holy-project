module {{moduleName}}.Coconut (coconut,coconutfunc,CoconutDataStruct(..)) where
data CoconutDataStruct = CoconutConstr [Integer] deriving (Show)

coconut :: Integer
coconut = 10

coconutfunc :: CoconutDataStruct -> Int
coconutfunc (CoconutConstr l) = length l
