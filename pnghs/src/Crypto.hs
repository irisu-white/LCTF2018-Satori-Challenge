module Crypto (encryptoData) where

import Data.Bits
import Data.Char
import Control.Monad.State.Lazy
import Control.Monad.Writer.Lazy

import Helper

{- Static Vaule -}
initIV :: [Int]
initIV = map ord "1fb7bec158785d11f4b27c1b46230d99"

initKey :: [Int]
initKey = map ord "UPRPRC"

blockLen :: Int
blockLen = 32

paddingData :: Int
paddingData = 5

{- Helper Function -}

paddingX :: Int -> a -> [a] -> [a]
paddingX n c d = d ++ replicate n c

{- Encrypto -}

type Calc = State [Int] [Int]

mulMod :: [Int] -> Calc
mulMod key = gets (\s -> zipWith f s $ cycle key)
    where f k m = k * m `mod` 256

xorKey :: [Int] -> Int -> Calc
xorKey key n = gets (\s -> map (xor n) $ take (length s) $ cycle key)

calcData :: Calc
calcData = do
    x <- mulMod initKey
    y <- xorKey initKey 5
    return $ zipWith xor x y

encryption :: [Int] -> [Int]
encryption d = evalState calcData d

{- Block Crypto -}

type DataBlock = Writer [Int] [Int]

initBlock :: [Int] -> DataBlock
initBlock iv = return iv

-- OFB Mode
encodeBlock :: ([Int] -> [Int]) -> [Int] -> DataBlock -> DataBlock
encodeBlock f msg db = db >>= (\iv -> writer (f iv, zipWith xor msg $ f iv ))

{- IO -}

foldData :: [[Int]] -> DataBlock
foldData list = foldr (encodeBlock encryption) (initBlock initIV) list

setData :: [Int] -> [[Int]]
setData d = warpList blockLen $
    paddingX ((-) blockLen $ length d `mod` blockLen) paddingData d

encryptoData :: [Int] -> [Int]
encryptoData d = execWriter $ foldData $ setData d


