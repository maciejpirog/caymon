module AlgebraicDatatype where

import Polynomial
import Theory

import Data.Char (toUpper)
import Data.List (nub)

type Identifier = String

data Type = TypeVar Identifier
          | TypeConsApp Identifier [Type]
          | TypeArr Type Type
          | TypeForall Identifier Type

data Constructor =
    Constructor { constructorName :: Identifier
                , constructorType :: [Type]
                }

data AlgebraicDatatype = AlgebraicDatatype
  { adtName :: Identifier
  , typeVars :: [Identifier]
  , constructors :: [Constructor]
  }
 
upFirst :: String -> String
upFirst (c:cs) = toUpper c : cs

adtFromTheory :: Polynomial -> Theory -> [AlgebraicDatatype]
adtFromTheory p t = tupleAdt p : kindsAdts
 where
  kindNames = do m <- monomials p
                 k <- replicate (coeff m) m
                 return $ kindName k
  kNames = nub kindNames
  outKindName k = "Out" ++ k
  outConsName k i = "Out" ++ show i
  whichKinds x = map fst $ filter (\(_,n) -> n == x)
                         $ zip [1..] kindNames
  varName = "v"
  tupleAdt p =
    AlgebraicDatatype
      (upFirst $ polyName p)
      [varName]
      [Constructor "Cons" $
         map (\k -> TypeConsApp (upFirst k) [TypeVar varName]) kindNames]
  kindsAdts = do
    k <- kNames
    [AlgebraicDatatype
      (upFirst k)
      [varName]
      (kindsAdt k $ filter (\o -> opCodom o == k) $ thOps t),
     AlgebraicDatatype
      (outKindName $ upFirst k)
      [varName]
      [Constructor (outConsName (upFirst k) i)
                   [TypeVar varName]
        | i <- whichKinds k]]
  kindsAdt k (o:os)
    | opType o == OTEpsilon =
        Constructor
          (upFirst (opName o))
          []
          : kindsAdt k os
    | opType o == OTGamma =
        Constructor
          (upFirst (opName o))
          (TypeConsApp (outKindName $ upFirst $ head $ opDom o)
                       [TypeVar varName]
            : replicate (length (opDom o) - 1)
                        (TypeConsApp (upFirst k) [TypeVar varName]))
        : kindsAdt k os
    | otherwise = kindsAdt k os
  kindsAdt _ [] = []
    
