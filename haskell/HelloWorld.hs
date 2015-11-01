{- HelloWorld.hs
   This is a multiline comment.
-}
-- This is a single-line comment.

hello :: IO ()
hello = putStrLn "Hello, world!"

main :: IO ()
main = do
     hello
