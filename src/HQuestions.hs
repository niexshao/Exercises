module HQuestions where

h1 :: [a] -> a
h1 = last

h2 :: [a] -> a
h2 = last . init

h3 :: [a] -> Int -> a
h3 xs n = xs !! (n-1)

h4 :: [a] -> Int
h4 = foldr (const (+1)) 0

h5 :: [a] -> [a]
h5 = foldl (\acc x -> x : acc) []

h6 :: Eq a => [a] -> Bool
h6 xs = xs == h5 xs 

h7 :: (Monoid a, Foldable t) => t a -> a
h7 = foldr mappend mempty
data NestedList a = Elem a | List [NestedList a]
