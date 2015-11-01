This is a Haskell toy to visualize twos-complement binary numbers.
In my previous articles on the subject of integer division, I alluded
two underflow and overflow and the "weird number" -- well, now you
can see them! 

Almost 30 years ago I wrote code on my Radio Shack TRS-80 to make my 
dot-matrix printer spit out bitmaps of binary numbers, which made 
pretty recursive patterns. This program commemorates that long-ago 
geekery from my early teenage hacking days.

First, we need the bits library:

>import Data.Bits

We'll also need the list library later:

>import List

Let's simulate n-bit binary number in both a signed and unsigned
interpretation. Given a number of bits, we want a function to make
a list of the possible values. For example, in unsigned form 8 bits 
will give us a list of the values 0..255; in signed form, it will be 
an asymmetrical list from -128..127. Note that this function generates
the whole list, so if you give it a numbits value higher than, say,
16, you might regret it.

Our calculation fails for values of numbits <= 0, so we put in a
guard case for that. We also want to discourage creating very large
lists, so we put in a guard case for numbits > 16.

>gen_n_bit_nums_unsigned numbits | numbits <= 0 = []
>gen_n_bit_nums_unsigned numbits | numbits > 16 = error "too many bits!"
>gen_n_bit_nums_unsigned numbits = 
>   [ n | n <- [(0::Int)..((2::Int)^numbits - 1)] ]

>gen_n_bit_nums_signed numbits | numbits <= 0 = [] 
>gen_n_bit_nums_signed numbits | numbits > 16 = error "too many bits!"
>gen_n_bit_nums_signed numbits = 
>   [ n | n <- [-(2::Int)^(numbits - 1)..((2::Int)^(numbits - 1) - 1)] ]

To test this we can execute:

gen_n_bit_nums_unsigned 4
[0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15]

gen_n_bit_nums_signed 4
[-8, -7, -6, -5, -4, -3, -2, -1, 0, 1, 2, 3, 4, 5, 6, 7]

That looks about right.

Now we want a function to access the bits of those numbers from most 
significant to least significant and build a list of corresponding 
boolean values. This would be expressed more naturally if EnumFromTo
operated descending sequences. I'm sure there's a better way, but for
now I will just reverse the resulting list:

>rightmost_bits num_bits val | num_bits > 16 = error "too many bits"
>rightmost_bits num_bits val | num_bits <= 0 = error "too few bits"
>rightmost_bits num_bits val = 
>   reverse [ testBit (val::Int) bit_idx | bit_idx <- [0..(num_bits - 1)] ]

Once we have a function which will do it with one number, doing it with a 
whole list of numbers is very easy. We will make a curried function and
use map. Currying in Haskell is very easy, which makes sense given that
the operation, "to curry" and the language, Haskell, are both named after
Haskell Curry!

Our rightmost_bits function takes two arguments, but we want to make a 
new function which we can hand off as an argument to map, along with a 
list. This new function will only take one parameter. We can ask GHCi 
what it thinks the type of the rightmost_bits function is:

:type rightmost_bits

It says:

rightmost_bits :: Int -> Int -> [Bool]

We can make a new function out of it by doing partial application:
instead of supplying all its arguments, we just supply the first one.

gen_rightmost_bits 4

If you do this in GHCi, you'll get an error message because the
result is not printable (GHCi can't apply show to the result). 
But you can bind it:

let bit_generator_4 = gen_rightmost_bits 4

And then ask GHCi what its type is:

:type bit_generator_4
bit_generator_4 :: [Int] -> [[Bool]]

Take a moment to make sure you understand what we've done here: 
Haskell has made us a curried function which takes a list of values 
instead of a single value, and generates a list of lists. This
seems pretty close to magic! Let's make another function that
takes the number of bits and a list, generates the curried
function, then maps the list to the curried function:

>gen_rightmost_bits num_bits ls = 
>   map (rightmost_bits num_bits) ls

As you can see, working with curried functions is completely
effortless in Haskell. Here's a general function to generate
a list of all the bit values for unsigned numbers of num_bits 
size:

>gen_all_bits_unsigned num_bits = 
>   gen_rightmost_bits num_bits (gen_n_bit_nums_unsigned num_bits)

>gen_all_bits_signed num_bits = 
>   gen_rightmost_bits num_bits (gen_n_bit_nums_signed num_bits)

If we execute:

gen_all_bits_unsigned 4

We get back the following (I have reformatted the output for clarity):

[[False, False, False, False], 
[False, False, False, True], 
[False, False, True, False],
[False, False, True, True],
[False, True, False, False],
[False, True, False, True],
[False, True, True, False],
[False, True, True, True],
[True, False, False, False],
[True, False, False, True],
[True, False, True, False],
[True, False, True, True],
[True, True, False, False],
[True, True, False, True],
[True, True, True, False],
[True, True, True, True]]

This is recognizably a table of the unsigned binary values 0..15.
This approach doesn't seem very "Haskell-ish" yet -- instead of 
generating the combinations as needed, I'm producing them all 
ahead of time. It also feels to me like I'm pushing uphill on 
the syntax, and having to use parentheses to force the order 
of evaluation. We can think more about that later, but let's 
get our pretty-printing working first.

How can I turn all those boolean values into a string I can print?
Well, we can take advantage of the fact that in Haskell, strings can
be treated as lists, and the same methods apply. This is not efficient,
but it allows us to use some of the standard functional techniques.
One of these is "fold."

Fold is a technique for boiling down a list into some kind of summary
form. Just what form this summary takes is up to you, since you can
supply your own function and a starting value. For example, to sum
the elements in a list, I could use foldl like this:

foldl (\ x y -> x + y) 0 [0, 1, 2]
 
The first parameter to foldl is a function. In this case I'll use
a lambda expression, which gives me a function I won't bother to
name. The second paramter is the starting value. The starting value
and the next item in the list are repeatedly passed to the function,
which accumulates a value. In this case, the final result is 3.

I can give foldl an empty string as the starting accumulator value
and my function can concatenate strings onto it: 

>stringify_vals =
>   foldl (\ x y -> if y then x ++ "#" else x ++ " ") ""

Now you might want to try to apply our list of lists to this
function:

stringify_vals (gen_all_bits_unsigned num_bits)

If you do that, though, you'll get a type error like this:

"Couldn't match expected type `Bool' against inferred type `[Bool]'"

The reason can be seen if we examine the types:

:type (gen_all_bits_unsigned 4) :: [[Bool]]
(gen_all_bits_unsigned 4) :: [[Bool]]

:type stringify_vals
stringify_vals :: [Bool] -> [Char]

We have a list of lists of bools, but we want to feed our
stringification function a list of bools. To apply each
element of (gen_all_bits_unsigned 4) to our stringification
function, we can use map again:

>stringify_all_unsigned_vals num_bits =
>   map stringify_vals (gen_all_bits_unsigned num_bits)

>stringify_all_signed_vals num_bits =
>   map stringify_vals (gen_all_bits_signed num_bits)

This will give us a list of strings, one for each list of boolean
values. (I've reformatted the output).

["    ",
"   #",
"  # ",
"  ##",
" #  ",
" # #",
" ## ",
" ###",
"#   ",
"#  #",
"# # ",
"# ##",
"##  ",
"## #",
"### ",
"####"]

That's nice: we can start to see our recursive structure! But
we really want that value to be turned into a single printable
string. To achieve that, we can use the unlines function.

>prettyprint_unsigned num_bits =
>   unlines (stringify_all_unsigned_vals num_bits)

>prettyprint_signed num_bits =
>   unlines (stringify_all_signed_vals num_bits)

When we interpret 

prettyprint_unsigned 3

we see a single string with newline characters escaped, like so:

"   \n  #\n # \n ##\n#  \n# #\n## \n###\n"

This will give the results we want when we pass it to putStr, like so:

putStr (prettyprint_unsigned 8)

        #
       #
       ##
      #
      # #
      ##
      ###
     #
     #  #
     # #
     # ##
     ##
     ## #
     ###
     ####

    (etc.)
    
Hmmm. That's nice, but it would be really cool if the results
were rotated, so that we could see what it would look like if
a printhead moving horizontally was printing out these values.
To do this, we can actually start with our intermediate list of
lists. If our four-bit numbers give us

[[False, False, False, False], 
[False, False, False, True], 
[False, False, True, False],
etc.

we can see the values the other way using transpose. Let's write
rotated versions of gen_all_bits_unsigned and gen_all_bits_signed:

>gen_all_bits_unsigned_rot num_bits = 
>   transpose
>   (gen_rightmost_bits num_bits (gen_n_bit_nums_unsigned num_bits))

>gen_all_bits_signed_rot num_bits = 
>   transpose
>   (gen_rightmost_bits num_bits (gen_n_bit_nums_signed num_bits))

And make a new stringify function to play with:

>stringify_all_unsigned_vals_rot num_bits =
>   map stringify_vals (gen_all_bits_unsigned_rot num_bits)

>prettyprint_unsigned_rot num_bits =
>   unlines (stringify_all_unsigned_vals_rot num_bits)

Now

putStr (prettyprint_unsigned_rot 6)

will give us:

                                 ################################
                 ################                ################
         ########        ########        ########        ########
     ####    ####    ####    ####    ####    ####    ####    ####
   ##  ##  ##  ##  ##  ##  ##  ##  ##  ##  ##  ##  ##  ##  ##  ##
  # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
 
Now that's what I'm talking about!
 
We can use the unsigned version to see the interesting transition
point between negative and positive values:
 
>stringify_all_signed_vals_rot num_bits =
>   map stringify_vals (gen_all_bits_signed_rot num_bits)

>prettyprint_signed_rot num_bits =
>   unlines (stringify_all_signed_vals_rot num_bits)

Now

putStr (prettyprint_signed_rot 6)

will give us:

 ################################
                 ################                ################
         ########        ########        ########        ########
     ####    ####    ####    ####    ####    ####    ####    ####
   ##  ##  ##  ##  ##  ##  ##  ##  ##  ##  ##  ##  ##  ##  ##  ##
  # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

Now let's give our program a main function using the IO monad and
have it print out several of our results:

>main :: IO ()
>main = do
>   print "Unsigned values (8 bits)"
>   putStr (prettyprint_unsigned 8) 
>   print "Signed values (8 bits)"
>   putStr (prettyprint_signed 8) 
>   print "Unsigned values (6 bits) rotated"
>   putStr (prettyprint_unsigned_rot 6) 
>   print "Signed values (6 bits) rotated"
>   putStr (prettyprint_signed_rot 6)

After using GHCi's :cd and :load command to read this file, just
type:

main
