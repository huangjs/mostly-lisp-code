import Data.Char

-- chapter 7 higher order functions

type Bit = Int

bin2int :: [Bit] -> Bit
bin2int = foldr (\x y -> x + 2 * y) 0

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

unfold p h t x | p x       = []
               | otherwise = h x : unfold p h t (t x)                       

int2bin = unfold (== 0) (`mod` 2) (`div` 2)

encode :: [Char] -> [Bit]
encode = concat . map (make8 . int2bin . ord)

chop8 :: [Bit] -> [[Bit]]
-- chop8 [] = []
-- chop8 x  = take 8 x : chop8 (drop 8 x)
chop8 = unfold null (take 8) (drop 8)

decode :: [Bit] -> [Char]
decode = map (chr . bin2int) . chop8

transmit :: [Char] -> [Char]
transmit = decode . channel . encode
  where channel = id

curry f = g
  where g x y = f (x, y)

iterateList :: Eq a => (a -> a) -> [a] -> [a]
iterateList f = unfold null head (tail . map f)

ziterate :: (a -> a) -> a -> [a]
ziterate f = unfold (\x -> False) id f

zmap :: Eq b => (b -> a) -> [b] -> [a]
zmap f = unfold null (f . head) tail

mmap :: (b -> a) -> [b] -> [a]
mmap f = foldl (\x y -> x ++ [f y]) []

ffilter :: (a -> Bool) -> [a] -> [a]
ffilter p = foldl (\x y -> x ++ if p y then [y] else []) []

dec2int :: [Integer] -> Integer
dec2int = foldl (\x y -> x * 10 + y) 0

compose :: [b -> b] -> b -> b
compose = foldr (.) id

-- sumsqreven = compose [sum, map (^ 2), filter even]
sumsqreven = sum . map (^ 2) . filter even

parity :: [Bit] -> Bit
parity = (`mod` 2) . sum

encode' :: [Char] -> [Bit]
encode' = concat . map ((\x -> parity x : make8 x) . int2bin . ord)

chop9 :: [Bit] -> [[Bit]]
chop9 = unfold null (take 9) (drop 9)

decode' :: [Bit] -> [Char]
decode' = map (chr . bin2int . checkParity) . chop9
  where checkParity (p:xs) | p == parity xs = xs
                           | otherwise     = error "Parity check error"

transmit' :: [Char] -> [Char]
transmit' = decode' . channel . encode'
  where channel = id


-- chapter 8 parsing

type Parser a = String -> [(a, String)]

zreturn :: a -> Parser a
zreturn v = \inp -> [(v, inp)]

failure :: Parser a
failure = \inp -> []

item :: Parser Char
item = \inp ->
  case inp of
    [] -> []
    (x:xs) -> [(x, xs)]
    
parse :: Parser a -> String -> [(a, String)]
parse p inp = p inp

(>>>=) :: Parser a -> (a -> Parser b) -> Parser b
p >>>= f = \inp -> 
  case parse p inp of
    [] -> []
    [(v, out)] -> parse (f v) out
    
instance Monad Parser where
  return = zreturn
  (>>=)  = (>>>=)
  fail   = failure

p :: Parser (Char, Char)
p = do x <- item
       item 
       y <- item
       return (x, y)

(+++) :: Parser a -> Parser a -> Parser a
p +++ q = \inp -> 
  case parse p inp of
    [] -> parse q inp
    [(v, out)] -> [(v, out)]
    
sat :: (Char -> Bool) -> Parser Char    
sat p = item >>>= \x -> if p x then zreturn x else failure
           
digit   :: Parser Char
digit   = sat isDigit
lower   :: Parser Char
lower   = sat isLower
upper   :: Parser Char
upper   = sat isUpper
letter  :: Parser Char
letter  = sat isAlpha
alphanum :: Parser Char
alphanum = sat isAlphaNum
char    :: Char -> Parser Char
char x  = sat (== x)

int :: Parser Int
int = do char '-' 
         n <- char
         
