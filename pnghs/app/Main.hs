module Main where

import System.IO
import Data.Char

import Crypto
import Image


inputPath = "/home/drimtuer/Code/LCTF2018/pnghs/input.png"
outputPath = "/home/drimtuer/Code/LCTF2018/pnghs/output.png"
plainText = "ThisIsTestData"


main :: IO ()
main = encodePicture inputPath outputPath $ encryptoData $ map ord plainText



