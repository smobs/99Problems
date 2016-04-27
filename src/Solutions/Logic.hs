module Solutions.Logic 
where

foo = "hlel"

and2 :: Bool -> Bool -> Bool
and2 True  True = True
and2 _ _ = False

infixl 4 ||||

(||||) = or2 

or2 False False = False
or2 _ _ = True

nand2 True True = False
nand2 _ _ = True

nor2 False False = True
nor2 _ _ = False

xor2 True False  = True 

xor2 False True  = True
xor2 _ _  = False

impl True False = False
impl _ _ = True

equiv :: Bool -> Bool -> Bool
equiv = (==)


table3 :: (Bool -> Bool -> Bool) -> [[Bool]]
table3 f =  [[a , b , f a b] | a <- [True, False], b <- [True, False]]

table4 :: [[Bool]] -> IO ()
table4 = mapM_ (\x -> putStrLn  (show x) ) 

table5 :: (Bool -> Bool -> Bool) -> IO ()
table5 = table4 . table3 
