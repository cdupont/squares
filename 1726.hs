-- math > info : recherche du minimum de S^2 = n^2 + 46 r^2
-- avec aussi n^2 + 23 r^2 = T^2.
-- Compilation :   ghc -O2 -Wall MinBilles.hs
-- Exécution :     ./MinBilles               (cherche jusqu’à trouver le minimum)
-- Optionnel :     ./MinBilles 100000        (borne max sur r pour couper tôt)

{-# LANGUAGE CPP          #-}
{-# LANGUAGE MagicHash    #-}

module Main where

import Data.List (foldl')
import System.Environment (getArgs)
import Data.Maybe
import GHC.Exts (Int(..), Word(..), Ptr(..), iShiftRL#, indexWordOffAddr#)
import Data.Bits (countTrailingZeros, finiteBitSize, testBit, (.&.))
import Math.NumberTheory.Roots
import Control.Monad (sequence)
--import Control.Parallel.Strategies (Strategy, using, rseq, rparWith, parMap)
import Debug.Trace (traceShow)
import System.IO (hFlush, hSetBuffering, BufferMode(NoBuffering), stdout)
import GHC.Utils.Misc
import Math.NumberTheory.Roots.Squares
import Math.NumberTheory.Utils.BitMask


nums max = [(a, b) | a <- [1..max], b <- [1..max]]


sqTest (a, b) = 
                indexBitSet mask256 (fromInteger (s2 .&. 255)) 
             && indexBitSet mask256 (fromInteger (s1 .&. 255)) 
             && indexBitSet mask693 (fromInteger (s2 `rem` 693))
             && indexBitSet mask693 (fromInteger (s1 `rem` 693))
             && indexBitSet mask325 (fromInteger (s2 `rem` 325))
             && indexBitSet mask325 (fromInteger (s1 `rem` 325))
             && snd (integerSquareRootRem' s1) == 0
             && snd (integerSquareRootRem' s2) == 0 
             where
                a2 = a * a
                b2 = b * b
                s1 = a2 + 46 * b2
                s2 = b2 + 23 * b2
  



main = do
  hSetBuffering stdout NoBuffering
  args <- getArgs
  let maxR = case args of
               (x:_) -> read x :: Integer
               _     -> error "not int" 

  --let posSqr = parMap' 1000 rseq (\(a, b) -> isPossibleSquare $ a*a + 46 * b*b) (nums maxR)
  --let posSqr' = filterByList posSqr (nums maxR)
  --
  --let s1 = filter (\(a, b) -> isPossibleSquare $ a*a + 46 * b*b) (nums maxR)
  --let s2 = filter (\(a, b) -> isSquare $ a*a + 23 * b*b) s1 
  --let s3 = filter (\(a, b) -> isSquare $ a*a + 46 * b*b) s2
   
  putStrLn $ "Solutions: " ++ (show $ filter sqTest $ nums maxR)


coords :: Integer -> (Integer, Integer)
coords n
  | n < 1     = error "n must be >= 1"
  | otherwise =
      let s = isqrt (n - 1)            -- ring index (max x or y)
          t = n - (s*s + 1)            -- offset along the ring (0..2s)
      in if t <= s
           then (s, t)                 -- right edge: (s, 0..s)
           else (s - (t - s), s)       -- top edge:  (s-1..0, s)


mask256 :: Ptr Word
mask256 = Ptr "\DC3\STX\ETX\STX\DC2\STX\STX\STX\DC3\STX\STX\STX\DC2\STX\STX\STX\DC2\STX\ETX\STX\DC2\STX\STX\STX\DC2\STX\STX\STX\DC2\STX\STX\STX"#

mask693 :: Ptr Word
mask693 = Ptr "\DC3\STXA\STX0\NUL\STX\EOTI\NUL\STX\t\CAN\NUL\NULB\164\NUL\DC1\EOT\b\STX\NUL@P\128@\NUL\STX\t\128 \SOH\DLE\NUL\SOH\130$\NUL\128\DC4(\NUL\NUL\SOH\DC2\NUL\f\STX\DC4\SOH\NUL \b\NUL\"\NUL\128\EOT`\144\NUL\b\129\NULE\DC2\DLE@\STX\EOT\NUL\129\NUL\t\b\EOT\SOH\194\128\NUL\DLE\EOT\NUL\DLE\NUL\NUL"#

mask325 :: Ptr Word
mask325 = Ptr "\DC3B\SOH&\144\NUL\n!%\140\STXH0\SOH\DC4BJ\b\ENQ\144@\STX(\132\148\DLE\n \131\EOTP\f)!\DC4@\STX\EM\160\DLE\DC2"#


--evalList' :: Integer -> Strategy a -> Strategy [a]
--evalList' t s as = sequence $ foldr f [] $ zip as [1..]
--  where f (a, n) ss | n `mod` t == 0 = s (traceShow n a):ss
--                    | otherwise      = s a:ss
--
--parList' :: Integer -> Strategy a -> Strategy [a]
--parList' t s = evalList' t (rparWith s)
--
--parMap' :: Integer -> Strategy b -> (a -> b) -> [a] -> [b]
--parMap' t s f xs = map f xs `using` parList' t s

