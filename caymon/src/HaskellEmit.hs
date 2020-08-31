module HaskellEmit where

import Polynomial
import Theory
import GenerateTheory
import AlgebraicDatatype

import Data.List (intersperse, sort, nub)
import Data.Char (toUpper)
import Data.Maybe (fromJust)
import Data.Monoid

--------------------------
-- General Haskell emit --
--------------------------

type Name = String

(<=>) :: String -> String -> String
e1 <=> "" = e1
e1 <=> (' ':cs) = e1 <=> cs
e1 <=> e2 = e1 <> " " <> e2

newline :: String
newline = "\n"

indent :: Int -> String -> String
indent n e =
  unlines
  $ map (\s -> replicate n ' ' ++ s)
  $ lines
  $ emitterToString e

emitterToString :: String -> String
emitterToString = id

emitStringList :: [String] -> String -> String
emitStringList []     sep = ""
emitStringList [s]    sep = s
emitStringList (s:ss) sep = s <> sep <> emitStringList ss sep

moduleHeader :: Name -> String
moduleHeader n = "module" <=> n <=> "where"
                 <=> newline <=> newline

languageExtension :: Name -> String
languageExtension n = "{-# LANGUAGE" <=> n <=> "#-}"
                        <> newline

importModule :: Name -> String
importModule n = "import" <=> n <> newline

classHeader :: Name -> Name -> String
classHeader n t =
  "class" <=> n <=> t <=> "where" <> newline

classHeaderFunDeps :: Name -> [Name] -> [(Name, Name)] -> String
classHeaderFunDeps cn n fds =
  "class" <=> cn <=> (concat $ intersperse " " n)
          <=> emitFunDeps <=> "where" <> newline
 where
  emitFunDeps | null fds  = ""
              | otherwise = "|" <=> aux fds
  aux []         = ""
  aux ((l,r):ds) = emitArrow (l) (r) <>
    (if null ds then "" else ",") <=> aux ds 

classBody :: String -> String
classBody e = indent 2 e

instanceHeader :: Name -> [String] -> String
instanceHeader cName args = "instance" <=> cName <=>
  emitStringList args " " <=> "where" <> newline

haddockComment :: String -> String
haddockComment s = "-- |" <=> s <> newline 

haddockHeader :: String -> String
haddockHeader s = newline <> "-- *" <=> s <> newline
                          <> replicate (length s + 5) '-' <> newline <> newline

haddockBlockComment :: String -> String
haddockBlockComment s = "{- | " <> s <> "-}" <> newline

emitTypeHeader :: Name -> [Name] -> String
emitTypeHeader n ns = "type" <=> n

emitFunType :: Name -> String -> String
emitFunType n s = n <=> "::" <=> s

emitArrow :: String -> String -> String
emitArrow e1 e2 = e1 <=> "->" <=> e2

emitArrows :: [String] -> String -> String
emitArrows []     cod = cod
emitArrows [x]    cod = x <=> "->" <=> cod
emitArrows (x:xs) cod = x <=> "->" <=> emitArrows xs cod

emitFunClause :: Name -> String -> String -> String
emitFunClause n pat body = n <=> pat <=> "=" <=> body

----------------------
-- Module structure --
----------------------

data HaskellEmitMode = Code | Docs

emitHaskell :: HaskellEmitMode -> Bool -> Polynomial -> Theory -> String
emitHaskell mode quickcheck p t =
  (if quickcheck then quickcheckDisclaimer else "") <>
  (if quickcheck then "-- CayMon run in the QuickCheck mode, which inhibits the \"Safe\" pragma\n"
                 else languageExtension "Safe") <>
  languageExtension "MultiParamTypeClasses" <>
  languageExtension "FunctionalDependencies" <>
  languageExtension "Rank2Types" <>
  languageExtension "DeriveFunctor" <>
  (if quickcheck then languageExtension "DeriveGeneric" <>
                      languageExtension "DefaultSignatures"
                 else "") <>
  newline <>
  haddockBlockComment (moduleDesc p t) <>
  moduleHeader (nameOfClass p) <>
  importModule "Control.Monad (ap)" <>
  (if quickcheck
    then "\n-- Needed for QuickCheck:" <> newline <>
         importModule "Control.Monad (join)" <>
         importModule "GHC.Generics (Generic)" <>
         importModule "Generic.Random (genericArbitrary, genericArbitraryRec, (%), withBaseCase)" <>
         importModule "Test.QuickCheck (Arbitrary(..), quickCheck)"
    else "") <>
  newline <>
  --
  haddockHeader "Equational Theory" <>
  emitEquations mode t <>
  classHeaderFunDeps (nameOfClass p)
                     (thName t : thKinds t)
                     (makeFunDeps t) <>
  classBody (hOps t) <> newline <>
  --
  haddockHeader "Direct-style Monad" <>
  emitDatasDeriving quickcheck (adtFromTheory p t)
                    ((if quickcheck then ("Generic":) else id) ["Functor", "Show", "Eq"]) <>
  haddockComment "Instance obtained by orienting the \"beta\" equations" <>
  emitDirectInstanceHeader p t <>
  indent 2 (emitDirectInstance p t) <> newline <>
  emitDirectApplicativeHeader p t <>
  emitApplicativeFromMonad <> newline <>
  emitDirectMonadHeader p t <>
  emitDirectMonadReturn p t <> newline <>
  emitDirectMonadBindViaFold p t <> newline <>
  emitFoldX p t <> newline <>
  --
  haddockHeader "Cayley Representation" <>
  emitMasterSelector p t <>
  emitSelectors p t <> newline <> newline <>
  emitSelectorInstanceHeader p t <>
  classBody (emitSelectorInstance p t) <> newline <>
  emitToCayley p t <> newline <>
  emitFromCayley p t <> newline <> newline <>
  (if quickcheck then emitSRTest p t <> emitHomoTest p t else "") <>
  --
  haddockHeader "Continuation-like (\\\"Cayley\\\") Monad" <>
  haddockBlockComment (contLikeComment p) <>
  emitContLikeData p t <>
  emitContLikeFunctor p t <>
  emitContLikeApplicative p t <>
  emitContLikeMonad p t <>
  emitContToDirect p t <>
  emitDirectToCont p t <>
  --
  (if quickcheck then haddockHeader "Test all" <> emitTestAll
                 else "")

quickcheckDisclaimer :: String
quickcheckDisclaimer =
  "-- Known to compile with Stack resolver lts-12.11\n\n"

nameOfClass :: Polynomial -> String
nameOfClass p = "C" ++ showShortPolynomial "x" p

makeFunDeps :: Theory -> [(Name, Name)]
makeFunDeps t = do k <- thKinds t
                   [(thName t, k), (k, thName t)]

hOps :: Theory -> String
hOps t = aux (thOps t)
 where
  aux [] = ""
  aux ((Operation _ n [] c) : os) = emitFunType n (c) <> newline
                                                           <> aux os
  aux ((Operation _ n d c) : os) =
    emitFunType n (emitArrow (dom d) (c)) <> newline <> aux os
  dom (k : t@(_ : ks)) = emitArrow (k) (dom t)
  dom [k] = k

printHaskell :: HaskellEmitMode -> Bool -> Polynomial -> Theory -> String
printHaskell mode quickcheck p t =
  emitterToString (emitHaskell mode quickcheck p t)

moduleDesc :: Polynomial -> Theory -> String
moduleDesc p t =
  "Module: " <> nameOfClass p <> newline <>
  "Description: Algebraic theory represented by " <> rep <> newline <>
  "Stability: experimental" <> newline <>
  newline <>
  "This module defines the equational theory Cayley-represented by the type"
  <> newline <> newline <> 
  "> " <=> rep <>
  newline <> newline <>
  "It was generated by __CayMon__ version 1.0 (<http://pl-uwr.bitbucket.io/caymon>)" <> newline <> newline
 where
  rep = (showLongPolynomial "x" p) <=> "->" <=> "x"

renderTerm :: Bool -> Term -> String
renderTerm b (TVar s) = s
renderTerm b (TOp f xs) | null xs = renderOp f
                        | otherwise = renderOp f <=> renderArgs xs
 where
  renderArgs [] = ""
  renderArgs (x:xs) = renderArg x <=> renderArgs xs
  renderArg (t@(TOp _ [])) = renderTerm b t
  renderArg (t@(TVar _))   = renderTerm b t
  renderArg t              = "(" ++ renderTerm b t ++ ")"
  renderOp f = if b then "'" <> f <> "'"
                    else f

renderEquation :: Bool -> Equation -> String
renderEquation b (Equation _ l r) =
  renderTerm b l <=> "=" <=> renderTerm b r

emitEquations :: HaskellEmitMode -> Theory -> String
emitEquations mode t =
  haddockBlockComment ("Instances should satisfy the following laws:" <>
                       newline <> renderEqs mode (thEqs t))
 where
  renderEqs _    []     = ""
  renderEqs Docs (e:es) = "\n *  @" <> (renderEquation True e) <> "@"
                                    <> newline <> renderEqs Docs es
  renderEqs Code (e:es) = (renderEquation False e) <> newline
                                                   <> renderEqs Code es

-- emit datatypes

emitDatas :: [AlgebraicDatatype] -> String
emitDatas []     = ""
emitDatas (d:ds) = emitData d <> newline <> emitDatas ds

emitDatasDeriving :: Bool -> [AlgebraicDatatype] -> [String] -> String
emitDatasDeriving quickcheck []     cl = ""
emitDatasDeriving quickcheck (d:ds) cl = emitData d <> newline <>
  "  deriving (" <> emitStringList cl ", " <> ")" <>
  newline <> newline <>
  (if quickcheck then emitDataArbitrary d else "") <>
  emitDatasDeriving quickcheck ds cl

emitData :: AlgebraicDatatype -> String
emitData adt@(AlgebraicDatatype name vars cons) =
  "data" <=> name <=> emitVars vars <=> "="
              <=> emitConstructors adt cons
 where
  emitVars []     = ""
  emitVars (v:vs) = v <=> emitVars vs

emitConstructors :: AlgebraicDatatype -> [Constructor] -> String
emitConstructors adt [c]    = emitConstructor c
emitConstructors adt (c:cs) =
  emitConstructor c <> newline <> (replicate dataHeadLength ' ') <> "|" <=>
  emitConstructors adt cs
 where
  dataHeadLength =
    length "data" + 1 + length (adtName adt) + 1 + sum (map length $ typeVars adt) +
    length (typeVars adt)
emitConstructors adt []     = ""

emitConstructor :: Constructor -> String
emitConstructor (Constructor name ts) =
  name <=> (emitTypes ts)

emitTypes :: [Type] -> String
emitTypes []     = ""
emitTypes (t@(TypeConsApp _ (_:_)):ts) = "(" <> emitType t <> ")" <=> emitTypes ts
emitTypes (t@(TypeArr _ _):ts) = "(" <> emitType t <> ")" <=> emitTypes ts
emitTypes (t@(TypeForall _ _):ts) = "(" <> emitType t <> ")" <=> emitTypes ts
emitTypes (t:ts) = emitType t <=> emitTypes ts

emitType :: Type -> String
emitType (TypeVar x) = x
emitType (TypeConsApp name ts) = name <=> emitTypes ts
emitType (TypeArr t1@(TypeArr _ _) t2) =
  "(" <> emitType t1 <> ")" <=> "->" <=> emitType t2
emitType (TypeArr t1 t2) = emitType t1 <=> "->" <=> emitType t2
emitType (TypeForall x t) =
  "forall" <=> x <> "." <=> emitType t

typeInCons :: String -> Constructor -> Bool
typeInCons s (Constructor _ ts) = or (map typeInType ts)
 where
  typeInType (TypeVar t) = t == s
  typeInType (TypeConsApp t _) = t == s
  typeInType _ = False

recursive :: AlgebraicDatatype -> Bool
recursive adt = findInConstructors (constructors adt)
 where
  s = adtName adt
  findInConstructors [] = False
  findInConstructors (c:cs) = if typeInCons s c then True else findInConstructors cs

getNonRecursiveConstructor :: AlgebraicDatatype -> Constructor
getNonRecursiveConstructor adt = head $ filter (not . typeInCons (adtName adt)) (constructors adt)

-- Nothing if the type is not recursive
getBaseCase :: AlgebraicDatatype -> Maybe Constructor
getBaseCase adt | recursive adt = Nothing
getBaseCase adt | otherwise     = Just $ getNonRecursiveConstructor adt

emitDataArbitrary :: AlgebraicDatatype -> String
emitDataArbitrary adt =
  "instance" <=> emitClassAssumptions (typeVars adt) <=> "Arbitrary" <=> emitTypeName <=>
  "where" <> newline <>
  (if recursive adt 
    then "  arbitrary = genericArbitraryRec (" <>
         emitStringList (consWeights $ constructors adt) " % " <=> "% ())" <> newline <>
         "    `withBaseCase`" <=> consToBaseCase (getNonRecursiveConstructor adt) 
    else "  arbitrary = genericArbitrary (" <>
         emitStringList (consWeights $ constructors adt) " % " <=> "% ())") <>
  newline <> newline
 where
  emitClassAssumptions []  = ""
  emitClassAssumptions [x] = "Arbitrary" <=> x <=> "=>"
  emitClassAssumptions xs  = "(" <> emitStringList (map ("Arbitrary" <=>) $ typeVars adt) ", " <> ")"
  emitTypeName | null (typeVars adt) = adtName adt
               | otherwise           = "(" <> adtName adt <=> emitStringList (typeVars adt) " " <> ")"
  consWeights [] = []
  consWeights (Constructor ('U' : _) _ : cs) = "2" : consWeights cs
  consWeights (_                       : cs) = "1" : consWeights cs
  consToBaseCase (Constructor n []) = "(pure" <=> n <> ")"
  consToBaseCase (Constructor n ts) = "(" <> n <=> "<$>" <=>
                                      emitStringList (map (const "arbitrary") ts) " <*> " <> ")"
  

-- direct instance

emitDirectInstanceHeader :: Polynomial -> Theory -> String
emitDirectInstanceHeader p t = instanceHeader (nameOfClass p) args
 where
  args = tcon (upFirst $ thName t) : map (tcon . upFirst) (thKinds t)
  tcon a = "(" <> a <=> varName <> ")"
  varName = "v"

eqToDef :: Equation -> String
eqToDef e =
  (renderTerm False $ haskL $ eqL e) <=> "=" <=>
    (renderTerm False $ haskR $ eqR e)
 where
  termUpFirst (TOp f xs) = TOp (upFirst f) (map termUpFirst xs)
  termUpFirst (t@(TVar x)) = t
  haskL (t@(TVar x)) = t
  haskL (TOp f xs) = TOp f (map termUpFirst xs)
  haskR (t@(TVar x)) = t
  haskR (TOp f xs) = TOp (upFirst f) xs

opToDef :: String -> String
opToDef s =
  renderTerm False (TOp s []) <=> "=" <=>
    renderTerm False (TOp (upFirst s) [])

emitDirectInstance :: Polynomial -> Theory -> String
emitDirectInstance p t =
  emitStringList
    (map opToDef $ map opName $ filter (q . opType) $ thOps t)
    newline
  <> newline <>
  emitStringList
    (sort $ map eqToDef $ filter (p . eqType) $ thEqs t)
    newline
 where
  p x = x `elem` [EqPiCons, EqGammaEpsilonX, EqGammaGamma]
  q x = x `elem` [OTCons, OTEpsilon]

emitDirectMonadHeader :: Polynomial -> Theory -> String
emitDirectMonadHeader p t =
  instanceHeader "Monad" [upFirst $ thName t]

emitDirectApplicativeHeader :: Polynomial -> Theory -> String
emitDirectApplicativeHeader p t =
  instanceHeader "Applicative" [upFirst $ thName t]

emitApplicativeFromMonad :: String
emitApplicativeFromMonad =
  "  pure  = return" <> newline <>
  "  (<*>) = ap" <> newline

emitDirectMonadReturn :: Polynomial -> Theory -> String
emitDirectMonadReturn p t =
  "  return x" <=> "=" <=>
  (upFirst $ opName $ consOp) <=>
  "\n    " <>
  emitStringList
    (do (i,k) <- zip [1..] $ opDom consOp
        let o = gammaOp k
        return $ "(" <> upFirst (opName o) <=>
                 emitStringList
                   (((map (appToX . upFirst . opName)
                         (filter (\o -> opType o == OTPi) $ thOps t))
                     !! (i - 1)) : 
                    map (upFirst . opName)
                        (filter (\o -> opType o == OTEpsilon
                                    && opCodom o == k) $ thOps t)) " "
                 <>")") "\n    "
  <> newline
 where
  consOp = head $ filter (\o -> opType o == OTCons) $ thOps t
  gammaOp k = head $ filter (\o -> opType o == OTGamma
                                && opCodom o == k
                                && head (opDom o) == k) $ thOps t
  appToX s = "(" <> s <=> "x" <> ")"

emitDirectMonadBind :: Polynomial -> Theory -> String
emitDirectMonadBind p t =
  mainBind <> newline <>
  "   where" <> newline <>
  indent 4 pis <>
  indent 4 (emitStringList
    (map (emitKindBind p t) (thKinds t))
    "\n")
 where
  consOp = head $ getOps (Just [OTCons]) Nothing t
  mainBind =
    "  " <>
    (upFirst $ opName $ consOp) <=>
    emitStringList (map (("x"++) . show)
                        [1 .. (length $ opDom consOp)]) " " <=>
    ">>=" <=> "f" <=> "=" <=>
    (upFirst $ opName $ consOp) <=>
    "\n    " <>
    emitStringList (map mainBindArg $ zip [1..] monomialsLong) "\n    "
  mainBindArg (i,k) = "(bind" <> upFirst k <=> "x" <> show i <=> "f)"
  monomialsLong = do m <- monomials p
                     l <- replicate (coeff m) m
                     return $ kindName l
  pis = emitPis $ zip [1..] monomialsLong
  emitPis ks = emitStringList (map emitPi ks) "\n"
  emitPi (i,k) = "out" ++ upFirst k <=> "(Out" <> show i <=> "x)" <=>
                 "=" <=> "out" ++ show i <=> "x"

emitDirectMonadBindViaFold :: Polynomial -> Theory -> String
emitDirectMonadBindViaFold p t =
  "  v >>= f =" <=> foldXName p t <=> "f" <=> "v" <> newline

emitKindBind :: Polynomial -> Theory -> String -> String
emitKindBind p t k = emitStringList
  (epsilons ++ comps) "\n"
 where
  funName = "bind" ++ upFirst k
  epsilonOps = filter (\o -> opCodom o == k && opType o == OTEpsilon)
                      (thOps t)
  epsilonClause s = emitFunClause funName (upFirst s <=> "_") (upFirst s)
  epsilons = map (epsilonClause . opName) epsilonOps
  comps = map gammaClause gammas
  gammas = getOps (Just [OTGamma]) (Just [k]) t
  gammaClause o = emitFunClause funName (gammaPattern o <=> "f") (gammaBody o)
  gammaPattern o = "(" <> upFirst (opName o) <=>
                   emitStringList ("x" : ys o) " " <> ")"
  gammaBody o = opName o <=> "(out" ++ upFirst (head $ opDom o) ++ " (fmap f x))" <=>
                emitStringList (map (appInY o) (ys o)) " "
  ys o = map (\i -> "y" ++ show i) [1 .. (length (opDom o) - 1)]
  appInY o s = "(" <> funName <=> s <=> "f" <> ")"

foldXName :: Polynomial -> Theory -> String
foldXName p t = "fold" ++ upFirst (polyName p)

emitFoldX :: Polynomial -> Theory -> String
emitFoldX p t =
  foldXType <> newline <>
  foldXClause <> newline <>
  " where" <> newline <>
  pis <> newline <>
  emitStringList (map foldA $ monomials p) "\n"
 where
  foldAName s = "fold" ++ upFirst s
  mainKind = polyName p
  kinds = map kindName $ monomials p
  tyCon t x = t <=> x
  longKinds = do m <- monomials p
                 replicate (coeff m) m
  foldXType = foldXName p t <=> "::" <=> nameOfClass p <=> mainKind <=>
              emitStringList kinds " " <=> "=>" <=>
              "(v ->" <=> mainKind <> ")" <=> "->" <=>
              tyCon (upFirst mainKind) "v" <=> "->" <=>
              mainKind
  foldXClause = foldXName p t <=> "f" <=> "(" <> upFirst consName <=>
                varyis (length longKinds) <> ")" <=> "=" <=>
                consName <=> emitStringList (map appFoldA $ zip [1..]
                                                          $ map kindName longKinds) " "
  appFoldA (i,s) = "(" <> foldAName s <=> varyi i <> ")"
  foldA m = emitStringList (foldAUnits m) "\n" <> newline <>
            emitStringList (foldAComps m) "\n"
  foldAUnits m = do o <- getOps (Just [OTEpsilon]) (Just [kindName m]) t
                    return $ "  " <> foldAName (kindName m) <=>
                             upFirst (opName o) <=> "=" <=> opName o
  foldAComps m = do o <- getOps (Just [OTGamma]) (Just [kindName m]) t
                    return  $ "  " <> foldAName (kindName m) <=> "(" <>
                              upFirst (opName o) <=> varx <=>
                              varyis (length (opDom o) - 1) <> ")" <=> "=" <=>
                              opName o <=>
                              "(out" <> upFirst (head $ opDom o) <=> varx <> ")" <=>
                              emitStringList (map appFoldA $ zip [1..]
                                                           $ tail $ opDom o) " "
  pis = emitPis $ zip [1..] $ map kindName longKinds
  emitPis ks = emitStringList (map emitPi ks) "\n"
  emitPi (i,k) = "  out" ++ upFirst k <=> "(Out" <> show i <=> "x)" <=>
                 "=" <=> "out" ++ show i <=> "(f x)"
  
-- Cayley representation

repArgName = "v"
selectorName = "FunRep"
kSelector n = selectorName ++ upFirst n
masterSelectorName = "FunRep"
varx = "x"
vary = "y"
varxi i = "x" ++ show i
varyi i = "y" ++ show i
varxis i = emitStringList (map (\n -> "x" ++ show n) [1..i]) " "
varyis i = emitStringList (map (\n -> "y" ++ show n) [1..i]) " "
vargi i = "g" ++ show i

selectorDatatype :: Monomial -> AlgebraicDatatype
selectorDatatype m = AlgebraicDatatype
  cName
  [repArgName]
  [Constructor cName [typeA $ power m]]
 where
  cName = kSelector (kindName m)
  typeA 0 = TypeVar repArgName
  typeA n = TypeArr (TypeVar repArgName) (typeA $ n-1)

emitSelectors :: Polynomial -> Theory -> String
emitSelectors p t =
  emitStringList
    (map (emitData . selectorDatatype) $ monomials p)
    "\n"

emitMasterSelector :: Polynomial -> Theory -> String
emitMasterSelector p t =
  emitData masterSelector <> newline
 where
  masterSelector =
    AlgebraicDatatype
      masterSelectorName [repArgName]
      [Constructor masterSelectorName selectors]
  selectors = do
    m <- monomials p
    l <- replicate (coeff m) m
    return $ TypeConsApp (kSelector $ kindName l) [TypeVar repArgName]

emitSelectorInstanceHeader :: Polynomial -> Theory -> String
emitSelectorInstanceHeader p t = instanceHeader (nameOfClass p) args
 where
  args = tcon masterSelectorName
           : map (\m -> tcon $ kSelector $ kindName m)
                 (monomials p)
  tcon s = "(" <> s <=> repArgName <> ")"

getKindArity :: String -> Polynomial -> Int
getKindArity k p = fromJust $ lookup k
                            $ map (\m -> (kindName m, coeff m))
                            $ monomials p

emitSelectorInstance :: Polynomial -> Theory -> String
emitSelectorInstance p t =
  consDef <> newline <>
  outDefs <> newline <>
  epsilonDefs <> newline <>
  gammaDefs <> newline
 where
  monomialsLong = do { m <- monomials p ; replicate (coeff m) m }
  consOp = head $ getOps (Just [OTCons]) Nothing t
  consDef = emitFunClause (opName consOp) [] masterSelectorName
  outOps = getOps (Just [OTPi]) Nothing t
  outDefs = emitStringList (map outDef $ zip [1..] outOps) "\n"
  outDef (i,o) = emitFunClause
                   (opName o)
                   ("(" <> masterSelectorName <=>
                     (varxis $ length $ monomialsLong) <> ")")
                   (varxi i)
  epsilonDefs = emitStringList
                  (concatMap epsilonDef $ monomials p)
                  "\n"
  epsilonDef m = do i <- [1 .. power m]
                    return $ emitFunClause
                      (epsilonName i m)
                      []
                      (kSelector (kindName m) <=> "(\\" <> varxis (power m)
                        <=> "->" <=> varxi i <> ")")
  gammaDefs = emitStringList (do m <- monomials p
                                 n <- monomials p
                                 return $ gammaDef m n) "\n"
  gammaDef m n =
    emitFunClause
      (gammaName m n)
      (emitStringList
        (tcon (kSelector $ kindName n) "f"
          : map (\i -> tcon (kSelector $ kindName m) (vargi i))
                [1 .. (power n)])
        " ")
      ((kSelector $ kindName m) <=> lam m n)
  tcon s x = "(" <> s <=> x <> ")"
  lam m n | power m > 0 =
    "(\\" <> (varxis $ power m) <=> "->" <=> "f" <=> apps m n  <> ")"
          | otherwise   =
    "(f" <=> apps m n <> ")"
  apps m n = emitStringList
               (map (\i -> "(" <> (vargi i) <=> (varxis $ power m) <> ")")
                    [1 .. (power n)])
               " "
emitToCayley :: Polynomial -> Theory -> String
emitToCayley p t =
  -- type
  "toCayley :: (C" <> showShortPolynomial "x" p <=> polyName p <=>
  emitStringList (map kindName $ monomials p) " " <> ")" <=> "=>" <=>
  polyName p <=> "->" <=> masterSelectorName <=> "x" <> newline <>
  -- binding
  "toCayley" <=> "x" <=> "=" <=> masterSelectorName <> "\n  " <>
  emitStringList cases "\n  " <> newline
 where
  monomialsLong = do m <- monomials p
                     replicate (coeff m) m
  cases =
    do (i,l) <- zip [1..] monomialsLong
       let beg = "(" <> kSelector (kindName l) <=> "(" <> emitLambda (varxis $ power l) <> opName consOp 
       return $ beg  <=>
         emitStringList (consArgs i l) ("\n" ++ replicate (length beg + 3 ) ' ') <>
         ")" <> ")"
  emitLambda [] = ""
  emitLambda xs =  "\\" <> xs <=> "-> "
  consOp = head $ getOps (Just [OTCons]) Nothing t
 -- gammaOp k = head $ filter (\o -> head (opDom o) == k)
 --                           (getOps (Just [OTGamma]) (Just [k]) t)
  consArgs i l =
    do (j,m) <- zip [1..] monomialsLong
       return $
         "(" <> gammaName m l <=>
         "(" <> piName i l <=> "x)" <=>
         
         emitStringList
           (map (\n -> "(" <> piName j l <=> "x" <> show n <> ")") [1 .. (power l)])
           " " <> ")"

emitFromCayley :: Polynomial -> Theory -> String
emitFromCayley p t =
  -- type
  "fromCayley :: (C" <> showShortPolynomial "x" p <=> polyName p <=>
  emitStringList (map kindName $ monomials p) " " <> ")" <=> "=>" <=>
  masterSelectorName <=> "x" <=> "->" <=> polyName p <> newline <>
  -- binding
  "fromCayley" <=> "(" <> masterSelectorName <=>
  emitStringList (map (\(i,m) -> "(" <> kSelector (kindName m) <=> "f" <> show i <> ")")
                      (zip [1..] monomialsLong)) " " <> ")" <=>
  "=" <=> (opName consOp) <> newline <> "  " <>
  emitStringList (map (\(i,m) -> "(" <> piName i m <=>
                                 "(" <> "f" <> show i <=>
                                 emitStringList
                                   (map (makeZero i m) [1..(power m)]) " " <>
                                 "))")
                      (zip [1..] monomialsLong))
                 "\n  " <> newline <>
  " where" <> newline <>
  "  __ = error \"This should have never happened, please contact the vendor!\""
 where
  makeZero j m i = "(" <> opName consOp <=> emitStringList (makeZeroArgs m i) " " <> ")"
  makeZeroArgs m i = do l <- monomialsLong
                        if kindName m /= kindName l
                          then return $ "__"
                          else return $ epsilonName i m 
  monomialsLong = do m <- monomials p
                     replicate (coeff m) m
  consOp = head $ getOps (Just [OTCons]) Nothing t

emitSRTest :: Polynomial -> Theory -> String
emitSRTest p t =
  "testFromCayleyToCayley :: IO ()" <> newline <>
  "testFromCayleyToCayley" <=> "= do" <> newline <>
  "  putStr \"(fromCayley . toCayley = id) for" <=> (upFirst $ thName t) <> ": \"" <> newline <>
  "  quickCheck prop" <> newline <>
  " where" <> newline <>
  "  prop ::" <=> (upFirst $ thName t) <=> "Int -> Bool" <> newline <>
  "  prop s = fromCayley (toCayley s) == s\n\n"

emitHomoTest :: Polynomial -> Theory -> String
emitHomoTest p t =
  "testToCayleyHomo :: IO ()" <> newline <>
  "testToCayleyHomo" <=> "= do" <> newline <>
  "  putStr \"" <> (upFirst $ thName t) <=> "to Cayley is a homomorphism: \"" <> newline <>
  "  quickCheck prop" <> newline <>
  " where" <> newline <>
  "  prop ::" <=> (upFirst $ thName t) <=> "(" <> (upFirst $ thName t) <=> "Int" <> ") -> Bool" <> newline <>
  "  prop s = fromCayley (" <> foldXName p t <=> "id (fmap toCayley s)) == join s\n\n"


-- Continuation-like monad

contLikeTypeName p = "CayMon" -- ++ showShortPolynomial "x" p

contLikeComment :: Polynomial -> String
contLikeComment p
  | functor (monomials p) =
     "Note that in '" <> masterSelectorName <> "' v the variable v" <=>
     "does not occur on the left-hand side of ->, which means that '" <>
     masterSelectorName <> "' is a functor. Hence, '" <> contLikeTypeName p <>
     "' is actually a codensity monad."
  | otherwise =
     "Note that in '" <> masterSelectorName <> "' v the variable v" <=>
     "occurs on the left-hand side of ->, which means that '" <>
     masterSelectorName <> "' is /not/ a functor. Hence, '" <> contLikeTypeName p <>
     "' is /not/ a codensity monad."
 where
  functor [] = True
  functor (Monomial _ _ 0 : ms) = functor ms
  functor _ = False

contLikeData :: Polynomial -> Theory -> AlgebraicDatatype
contLikeData p t = AlgebraicDatatype
  (contLikeTypeName p)
  [repArgName]
  [Constructor (contLikeTypeName p) [TypeForall "z" $
    TypeArr (TypeArr (TypeVar repArgName)
                     (TypeConsApp masterSelectorName [TypeVar "z"]))
            (TypeConsApp masterSelectorName [TypeVar "z"])]]

emitContLikeData :: Polynomial -> Theory -> String
emitContLikeData p t = (emitData $ contLikeData p t) <> newline <> newline

emitContLikeFunctor :: Polynomial -> Theory -> String
emitContLikeFunctor p t = instanceHeader "Functor" [contLikeTypeName p] <>
  "  fmap f (" <> contLikeTypeName p <=> "m)" <=> "=" <=> contLikeTypeName p <=> "(\\k -> m (\\x -> k (f x)))" <> newline <> newline

emitContLikeApplicative :: Polynomial -> Theory -> String
emitContLikeApplicative p t = instanceHeader "Applicative" [contLikeTypeName p] <>
  "  pure  = return" <> newline <>
  "  (<*>) = ap" <> newline <> newline

emitContLikeMonad :: Polynomial -> Theory -> String
emitContLikeMonad p t = instanceHeader "Monad" [contLikeTypeName p] <>
  "  return x =" <=> contLikeTypeName p <=> "(\\k -> k x)" <> newline <>
  "  m >>= k  =" <=> contLikeTypeName p <=> "(\\c -> run m (\\a -> run (k a) c))" <> newline <>
  "   where" <> newline <>
  "    run" <=> "(" <> contLikeTypeName p <=> "m) = m" <> newline <> newline

emitContToDirect p t =
  "contToDirect ::" <=> contLikeTypeName p <=>  "v" <=> "->" <=>
    upFirst (polyName p) <=> "v" <> newline <>
  "contToDirect (" <> contLikeTypeName p <=>
    "m) = fromCayley (m (\\v -> toCayley (return v)))" <> newline <> newline

emitDirectToCont p t =
  "directToCont ::" <=> upFirst (polyName p) <=> "v" <=> "->" <=>
    contLikeTypeName p <=>  "v" <> newline <>
  "directToCont x" <=> "=" <=> contLikeTypeName p <=> "(\\k ->" <=>
    "fold" <> upFirst (polyName p) <=> "k" <=> "x" <> ")" <> newline <> newline


-- Further tests

emitTestAll =
  "testAll :: IO ()" <> newline <>
  "testAll = do" <> newline <>
  "  testFromCayleyToCayley" <> newline <>
  "  testToCayleyHomo"
