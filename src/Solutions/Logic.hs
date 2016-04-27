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


table :: (Bool -> Bool -> Bool) -> [[Bool]]
table f =  [[a , b , f a b] | a <- [True, False], b <- [True, False]]

printTable :: [[Bool]] -> IO ()
printTable = mapM_ (\x -> putStrLn  (show x) ) 

truthTable :: (Bool -> Bool -> Bool) -> IO ()
truthTable = printTable . table 

perm 0 = [[]]
perm n = (map (\x -> True: x) (perm (n-1)))
               ++
           (map (\x -> False: x) (perm (n-1)))
           
tableN :: Int -> ([Bool] -> Bool) -> [[Bool]]
tableN n f =
    map (\x -> x ++ [f x]) (perm n) 

grey :: Int -> [[Bool]]
grey 0 = [[]]
grey n = (map (\x -> [True] ++ x) g ) ++ (map (\x -> [False] ++ x) (reverse g))
      where
         g = grey (n-1) 
