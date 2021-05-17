module Correctness where

import ParseMINI
import InterpretMINI
import Test.QuickCheck
import Text.Parsec(parse)

-- fibonacci

fib :: [Integer]
fib = 1 : 1 : zipWith (+) fib (tail fib)

fibonacci :: Int -> Integer
fibonacci n = fib !! n

miniProgFib :: String
miniProgFib = "procedure main (n) { a = 0; b = 1; m = 0; i = 1; while (i <= n) { m = a+b; a = b ; b = m; i = i + 1;} return b;}"

fibParsed :: Program
fibParsed = prog
        where (Right prog) = parse programParseEOF "" miniProgFib

prog1_corr :: Int -> Property
prog1_corr n = n >= 0 ==> fibonacci n == runProgram [toInteger n] fibParsed

-- primes

factor :: Int -> [Int]
factor n = [ i | i <- [1..n], mod n i == 0 ]

prime :: Int -> Bool
prime n = factor n == [1,n]

miniProgPrime :: String
miniProgPrime = "procedure main (n) { i = 1; p = 0; while (i <= n) { if ( n - (i * (n/i) ) == 0 ){ i = i+1; p = p+1;} else {i = i+1 ;} } if (p == 2) {result = 1;} else {result = 0;} return result; }"

primeParsed :: Program
primeParsed = prog
        where (Right prog) = parse programParseEOF "" miniProgPrime

prog2_corr :: Int -> Property
prog2_corr n = n >= 0 ==> (fromEnum (prime n)) == fromInteger (runProgram [toInteger n] primeParsed)

-- least common multiple

miniProgLCM :: String
miniProgLCM = "procedure main (a , b) {n = a; m = b; r = b ; if  ( a != 0) {while (b != 0) { if ( a <= b) {b = b - a ;} else {a = a - b ;} } r = a ; } if (r != 0) {result = (n*m) / r ; } else { result = 0; } return result ;}"

lcmParsed :: Program
lcmParsed = prog
        where (Right prog) = parse programParseEOF "" miniProgLCM

prog3_corr :: Int -> Int -> Property
prog3_corr n m = n >= 0 && m >= 0 ==> (lcm n m) == fromInteger (runProgram [toInteger n, toInteger m] lcmParsed)