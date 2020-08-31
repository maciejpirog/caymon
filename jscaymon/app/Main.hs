{-# LANGUAGE OverloadedStrings #-}

module Main where

import Polynomial
import Theory
import TheoryFromPolynomial
import HaskellEmit

import GHCJS.Types
import Data.JSString

foreign import javascript unsafe
  "$r = getData();"
  getData :: JSString

foreign import javascript unsafe
  "setData( $1 );"
  putData :: JSString -> IO ()

{-
pack :: String -> JSString
unpack :: JSString -> String
-}

main :: IO ()
main = do
 let input = unpack $ getData
 let p = parsePoly input
 let t = theoryFromPolynomial p
 let output = printHaskell Code p t
 putData $ pack output

{-
main :: IO ()
main = do
  let x = getD
  putD $ pack $ ("!!!!!!!" ++) $ unpack x
  return ()
-}
