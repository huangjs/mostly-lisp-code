jf1 :: a -> Bool
jf1 _ = True

jf2 :: a -> Bool
jf2 _ = True

jf :: a -> Bool
jf = and . zipWith ($) [jf1, jf2] . repeat

main :: IO ()
main = do
     putStrLn $ show $  jf 0
