-- math > info : recherche du minimum de S^2 = n^2 + 46 r^2
-- avec aussi n^2 + 23 r^2 = T^2.
-- Compilation :   ghc -O2 -Wall MinBilles.hs
-- Exécution :     ./MinBilles               (cherche jusqu’à trouver le minimum)
-- Optionnel :     ./MinBilles 100000        (borne max sur r pour couper tôt)

{-# LANGUAGE CPP          #-}
{-# LANGUAGE MagicHash    #-}

module Main where

import System.Environment (getArgs)
import Math.NumberTheory.Roots
import Math.NumberTheory.Roots.Squares
import Math.NumberTheory.Utils.BitMask
import UnliftIO.Async (pooledMapConcurrentlyN, pooledMapConcurrently)
import Control.Concurrent
import Control.Monad
import Control.Exception
import GHC.Exts (Ptr(..))
import Data.Bits ((.&.))

blockFile = "block"
solFile = "solution"
defaultSize = 10000
maxBlock = 1000000
defaultCap = 10

main :: IO ()
main = do
  content <- readFile blockFile
  let currentBlock = read content :: Integer
  args <- getArgs
  let (cap, size, start, end) = case args of
               (cap:si:st:nd:_) -> (read cap :: Int, read si :: Integer, read st :: Integer, read nd :: Integer)
               _ -> (defaultCap, defaultSize, currentBlock, maxBlock) 
  c <- getNumCapabilities 
  putStrLn $ "Num capabilities: " ++ (show c)
  putStrLn $ "Cap: " ++ (show cap) ++ ", block size: " ++ (show size) ++ ", start block " ++ (show start) ++ ", end block " ++ (show end)
  pooledMapConcurrentlyN cap id $ map (evBlockN size) [start..end]
  return ()

evBlockN :: Integer -> Integer -> IO ()
evBlockN size n = do
  let res = filter sqTest $ blockN n size
  putStrLn ("Block " ++ (show n) ++ ": " ++ (show res))
  if (res /= [])
    then do
      writeFile solFile (show res)
      error "Solution found!"
    else return ()
  _ <- (try $ writeFile blockFile (show n)) :: IO (Either SomeException ())
  return ()

blockN :: Integer -> Integer -> [(Integer, Integer)]
blockN n size = pairs where
   x = (fst $ blockCoords n) * size
   y = (snd $ blockCoords n) * size
   pairs = [(a, b) | a <- [x+1..x+size], b <- [y+1..y+size]]

blockCoords :: Integer -> (Integer, Integer)
blockCoords n
  | n < 1     = error "n must be >= 1"
  | otherwise =
      let s = integerSquareRoot (n - 1)            -- ring index (max x or y)
          t = n - (s*s + 1)            -- offset along the ring (0..2s)
      in if t <= s
           then (s, t)                 -- right edge: (s, 0..s)
           else (s - (t - s), s)       -- top edge:  (s-1..0, s)


sqTest :: (Integer, Integer) -> Bool
sqTest (a, b) = indexBitSet mask256 (fromInteger (s2 .&. 255)) 
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
                s2 = a2 + 23 * b2

mask256 :: Ptr Word
mask256 = Ptr "\DC3\STX\ETX\STX\DC2\STX\STX\STX\DC3\STX\STX\STX\DC2\STX\STX\STX\DC2\STX\ETX\STX\DC2\STX\STX\STX\DC2\STX\STX\STX\DC2\STX\STX\STX"#

mask693 :: Ptr Word
mask693 = Ptr "\DC3\STXA\STX0\NUL\STX\EOTI\NUL\STX\t\CAN\NUL\NULB\164\NUL\DC1\EOT\b\STX\NUL@P\128@\NUL\STX\t\128 \SOH\DLE\NUL\SOH\130$\NUL\128\DC4(\NUL\NUL\SOH\DC2\NUL\f\STX\DC4\SOH\NUL \b\NUL\"\NUL\128\EOT`\144\NUL\b\129\NULE\DC2\DLE@\STX\EOT\NUL\129\NUL\t\b\EOT\SOH\194\128\NUL\DLE\EOT\NUL\DLE\NUL\NUL"#

mask325 :: Ptr Word
mask325 = Ptr "\DC3B\SOH&\144\NUL\n!%\140\STXH0\SOH\DC4BJ\b\ENQ\144@\STX(\132\148\DLE\n \131\EOTP\f)!\DC4@\STX\EM\160\DLE\DC2"#

