-- in module Data.Tuple
pair :: (a -> b, a -> c) -> a -> (b, c)
pair (f, g) x  = (f x, g x)

cross :: (a -> b, c -> d) -> (a, c) -> (b, d)
cross (f, g) (x, y)    = (f x, g y)

