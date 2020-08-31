module Theory where

-- Operations

data OpType = OTCons | OTPi | OTEpsilon | OTGamma
  deriving (Eq, Show)

data Operation = Operation { opType  :: OpType
                           , opName  :: String
                           , opDom   :: [String]
                           , opCodom :: String
                           }
 deriving (Show)

-- Terms

data Term = TOp { tOp   :: String
                , tArgs :: [Term]
                }
          | TVar { tVar :: String }
 deriving (Show)

-- Equations

data EqType = EqConsPi | EqPiCons | EqGammaEpsilonX
            | EqGammaXEpsilon | EqGammaGamma
  deriving (Eq, Show)

data Equation = Equation { eqType :: EqType
                         , eqL    :: Term
                         , eqR    :: Term
                         }
 deriving (Show)

-- Theories

data Theory = Theory { thName  :: String
                     , thKinds :: [String]
                     , thOps   :: [Operation]
                     , thEqs   :: [Equation]
                     }
 deriving (Show)

-- Additional functions

getOps :: Maybe [OpType] -> Maybe [String] -> Theory -> [Operation]
getOps types kinds t = filter (\o -> typ o && kind o) $ thOps t
 where
  typ o = case types of
            Nothing -> True
            Just xs -> opType o `elem` xs
  kind o = case kinds of
            Nothing -> True
            Just xs -> opCodom o `elem` xs
