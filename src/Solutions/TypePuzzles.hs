module Solutions.TypePuzzles where

class Fluffy t where
  furry :: (a -> b) -> t a -> t b

-- Exercise 1
-- Relative Difficulty: 1
instance Fluffy [] where
  furry = map

-- Exercise 2
-- Relative Difficulty: 1
instance Fluffy Maybe where
  furry f Nothing = Nothing
  furry f (Just x) = (Just(f x))

-- Exercise 3
-- Relative Difficulty: 5
--   furry :: (a -> b) -> (t -> a) -> ( t -> b)
instance Fluffy ((->) t) where
  furry f g = f . g  

newtype EitherLeft b a = EitherLeft (Either a b)
newtype EitherRight a b = EitherRight (Either a b)

-- Exercise 4
-- Relative Difficulty: 5
-- data Either a b = Left a | Right b
instance Fluffy (EitherLeft t) where
  furry f (EitherLeft x) =
    case x of
    Right y -> EitherLeft (Right y)
    Left y -> EitherLeft (Left (f y))
    

-- Exercise 5
-- Relative Difficulty: 5
instance Fluffy (EitherRight t) where
  furry f (EitherRight x) = EitherRight $ case x of
    Left y -> Left y
    Right y -> Right $ f y

class Misty m where
  banana :: (a -> m b) -> m a -> m b
  unicorn :: a -> m a
  
  -- Exercise 6
  -- Relative Difficulty: 3
  -- (use banana and/or unicorn)
  furry' :: (a -> b) -> m a -> m b
  furry' f = banana (unicorn . f) 

-- Exercise 7
-- Relative Difficulty: 2
instance Misty [] where
  -- banana :: (a -> [b]) -> [a] -> [b]
  banana f x = concat(map f x)
  -- unicorn :: (a -> [a])
  unicorn x = [x]

-- Exercise 8
-- Relative Difficulty: 2
instance Misty Maybe where
  -- banana :: (a -> Maybe b) -> Maybe a -> Maybe b
  banana f (Just a) = f a 
  banana f Nothing = Nothing 
  -- unicorn :: (a -> Maybe a)
  unicorn a = Just a 

-- Exercise 9
-- Relative Difficulty: 6
instance Misty ((->) t) where
  -- banana :: (a ->  (t -> b)) -> (t -> a) -> (t -> b)
  banana f g = \t -> f (g t) t
  -- unicorn :: (a -> (t -> a))
  unicorn a = \x -> a

-- Exercise 10
-- Relative Difficulty: 6
instance Misty (EitherLeft t) where
  banana = error "todo"
  unicorn = error "todo"

-- Exercise 11
-- Relative Difficulty: 6
instance Misty (EitherRight t) where
  banana = error "todo"
  unicorn = error "todo"

-- Exercise 12
-- Relative Difficulty: 3
jellybean :: (Misty m) => m (m a) -> m a
jellybean = error "todo"

-- Exercise 13
-- Relative Difficulty: 6
apple :: (Misty m) => m a -> m (a -> b) -> m b
apple = error "todo"

-- Exercise 14
-- Relative Difficulty: 6
moppy :: (Misty m) => [a] -> (a -> m b) -> m [b]
moppy = error "todo"

-- Exercise 15
-- Relative Difficulty: 6
-- (bonus: use moppy)
sausage :: (Misty m) => [m a] -> m [a]
sausage = error "todo"

-- Exercise 16
-- Relative Difficulty: 6
-- (bonus: use apple + furry')
banana2 :: (Misty m) => (a -> b -> c) -> m a -> m b -> m c
banana2 = error "todo"

-- Exercise 17
-- Relative Difficulty: 6
-- (bonus: use apple + banana2)
banana3 :: (Misty m) => (a -> b -> c -> d) -> m a -> m b -> m c -> m d
banana3 = error "todo"

-- Exercise 18
-- Relative Difficulty: 6
-- (bonus: use apple + banana3)
banana4 :: (Misty m) => (a -> b -> c -> d -> e) -> m a -> m b -> m c -> m d -> m e
banana4 = error "todo"

newtype State s a = State {
  state :: (s -> (s, a))
}

-- Exercise 19
-- Relative Difficulty: 9
instance Fluffy (State s) where
  furry = error "todo"

-- Exercise 20
-- Relative Difficulty: 10
instance Misty (State s) where
  banana = error "todo"
  unicorn = error "todo"
