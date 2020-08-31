{-# LANGUAGE MultiWayIf #-}

module Polynomial where

import Control.Monad (liftM2)
import Data.Char
import Data.List

data Monomial = Monomial { kindName :: String
                         , coeff    :: Int
                         , power    :: Int
                         }
 deriving (Show)

data Polynomial = Polynomial { polyName  :: String
                             , monomials :: [Monomial]
                             }
 deriving (Show)

-- Printing, parsing, and naming

showShortPolynomial :: String -> Polynomial -> String
showShortPolynomial var (Polynomial _ []) = "0"
showShortPolynomial var p = aux var p
 where
  aux var (Polynomial _ []) = ""
  aux var (Polynomial _ (Monomial _ n e : ms)) =
    show n ++ var ++ show e ++ (if null ms then "" else "_")
      ++ aux var (Polynomial undefined ms)

showLongPolynomial :: String -> Polynomial -> String
showLongPolynomial var (Polynomial _ []) = "0"
showLongPolynomial var p = aux var p
 where
  aux var (Polynomial _ []) = ""
  aux var (Polynomial _ (Monomial _ n e : ms)) =
    show n ++ " * " ++ var ++ "^" ++ show e
      ++ (if null ms then "" else " + ")
      ++ aux var (Polynomial undefined ms)

parsePoly :: String -> Polynomial
parsePoly cs = renamePoly . truncatePoly $
  Polynomial "" (map parseMono . map splitMono . splitOnPlus . removeWS $ cs)
 where
  parseMono' (c,v,e) = Monomial "" (read c) (read e)
  parseMono (c,v,e) = parseMono' (defC c, "", defE v e)
  defC "" = "1"
  defC cs = cs
  defE v ('^':cs)  = defE v cs
  defE "" ""       = "0"
  defE _  ""       = "1"
  defE _ cs        = cs
  splitMono xs | null xs = error "Parse error: empty monomial!"
  splitMono xs = let (r,e) = break (=='^') xs
                     (c,v) = break (=='*') r
                  in if | null v && all isDigit c -> (c,"",e)
                        | null v && all isAlpha c -> ("",c,e)
                        | otherwise -> (c,v,e)
  splitOnPlus xs = case break (=='+') xs of
                     (p,"") -> [p]
                     (p,ps) -> p : splitOnPlus (tail ps)
  removeWS = filter (not . isSpace)

mainKind :: String
mainKind = "x"

kinds :: [String]
kinds = seed ++ addSeed kinds
 where
  seed = map return ['a'..'t']
  addSeed xs = liftM2 (++) xs seed
  
renamePoly :: Polynomial -> Polynomial
renamePoly (Polynomial _ ms) = Polynomial mainKind (zipWith renameMono kinds ms)
 where
  renameMono k m = m {kindName = k}

truncatePoly :: Polynomial -> Polynomial
truncatePoly p = p {monomials = filter nonzero (monomials p)}
 where
  nonzero m = coeff m > 0
