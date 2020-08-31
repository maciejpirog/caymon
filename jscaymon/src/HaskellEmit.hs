module HaskellEmit where

import Polynomial
import Theory
import TheoryFromPolynomial
import AlgebraicDatatype

import Data.List (intersperse, sort, nub)
import Data.Char (toUpper)
import Data.Maybe (fromJust)
import Data.Monoid

--------------------------
-- General Haskell emit --
--------------------------

type Name = String

type Emitter = String

-- instance Monoid Emitter where ...

emit :: String -> Emitter
emit = id

(<=>) :: Emitter -> Emitter -> Emitter
e1 <=> "" = e1
e1 <=> (' ':cs) = e1 <=> cs
e1 <=> e2 = e1 <> emit " " <> e2

newline :: Emitter
newline = emit "\n"

indent :: Int -> Emitter -> Emitter
indent n e = emit $ unlines
                  $ map (\s -> replicate n ' ' ++ s)
                  $ lines
                  $ emitterToString e

emitterToString :: Emitter -> String
emitterToString = id

emitStringList :: [String] -> String -> Emitter
emitStringList []  sep    = emit ""
emitStringList [s] sep    = emit s
emitStringList (s:ss) sep = emit s <> emit sep <> emitStringList ss sep

moduleHeader :: Name -> Emitter
moduleHeader n = emit "module" <=> emit n <=> emit "where"
                 <=> newline <=> newline

languageExtension :: Name -> Emitter
languageExtension n = emit "{-# LANGUAGE" <=> emit n <=> emit "#-}"
                        <> newline

importModule :: Name -> Emitter
importModule n = emit "import" <=> emit n <> newline

classHeader :: Name -> Name -> Emitter
classHeader n t =
  emit "class" <=> emit n <=> emit t <=> emit "where" <> newline

classHeaderFunDeps :: Name -> [Name] -> [(Name, Name)] -> Emitter
classHeaderFunDeps cn n fds =
  emit "class" <=> emit cn <=> emit (concat $ intersperse " " n)
               <=> emitFunDeps <=> emit "where" <> newline
 where
  emitFunDeps | null fds  = emit ""
              | otherwise = emit "|" <=> aux fds
  aux []         = emit ""
  aux ((l,r):ds) = emitArrow (emit l) (emit r) <>
    (if null ds then emit "" else emit ",") <=> aux ds 

classBody :: Emitter -> Emitter
classBody e = indent 2 e

instanceHeader :: Name -> [String] -> Emitter
instanceHeader cName args = emit "instance" <=> emit cName <=>
  emitStringList args " " <=> emit "where" <> newline

haddockComment :: Emitter -> Emitter
haddockComment s = emit "-- " <=> s <> newline 

haddockHeader :: String -> Emitter
haddockHeader s = emit "-- " <=> s <> newline <> newline

--haddockBlockComment :: Emitter -> Emitter
--haddockBlockComment s = emit "{- " <> s <> emit "-}" <> newline

haddockBlockComment :: Emitter -> Emitter
haddockBlockComment = unlines . map ("-- " ++) . lines

emitTypeHeader :: Name -> [Name] -> Emitter
emitTypeHeader n ns = emit "type" <=> emit n

emitFunType :: Name -> Emitter -> Emitter
emitFunType n s = emit n <=> emit "::" <=> s

emitArrow :: Emitter -> Emitter -> Emitter
emitArrow e1 e2 = e1 <=> emit "->" <=> e2

emitArrows :: [Emitter] -> Emitter -> Emitter
emitArrows [] cod = cod
emitArrows [x] cod = x <=> "->" <=> cod
emitArrows (x:xs) cod = x <=> "->" <=> emitArrows xs cod

emitFunClause :: Name -> String -> String -> Emitter
emitFunClause n pat body = emit n <=> pat <=> "=" <=> emit body

----------------------
-- Module structure --
----------------------

data HaskellEmitMode = Code | Docs

emitHaskell :: HaskellEmitMode -> Polynomial -> Theory -> Emitter
emitHaskell mode p t =
  languageExtension "Safe" <>
  languageExtension "MultiParamTypeClasses" <>
  languageExtension "FunctionalDependencies" <>
  languageExtension "Rank2Types" <>
  languageExtension "DeriveFunctor" <>
  newline <>
  --haddockBlockComment (moduleDesc p t) <>
  moduleHeader (nameOfClass p) <>
  importModule "Control.Monad (ap)" <> newline <>
  --
  haddockHeader "Equational Theory" <>
  classHeaderFunDeps (nameOfClass p)
                     (thName t : thKinds t)
                     (makeFunDeps t) <>
  classBody (hOps t) <> newline <>
  emitEquations mode t <> newline <>
  --
  haddockHeader "Direct-style Monad" <>
  emitDatasDeriving (adtFromTheory p t) ["Functor", "Show", "Eq"] <>
  haddockComment "Instance obtained by orienting the \"beta\" equations" <>
  emitDirectInstanceHeader p t <>
  indent 2 (emitDirectInstance p t) <> newline <>
  emitDirectApplicativeHeader p t <>
  emitApplicativeFromMonad <> newline <>
  emitDirectMonadHeader p t <>
  emitDirectMonadReturn p t <> newline <>
  --emitDirectMonadBind p t <> newline <>
  emitDirectMonadBindViaFold p t <> newline <>
  emitFoldX p t <> newline <> newline <>
  --
  haddockHeader "Cayley Representation" <>
  emitMasterSelector p t <>
  emitSelectors p t <> newline <> newline <>
  emitSelectorInstanceHeader p t <>
  classBody (emitSelectorInstance p t) <> newline <>
  emitToCayley p t <> newline <>
  emitFromCayley p t <> newline <> newline <>
  --
  haddockHeader "Continuation-like ('Cayley') Monad" <>
  haddockBlockComment (contLikeComment p) <>
  emitContLikeData p t <>
  emitContLikeFunctor p t <>
  emitContLikeApplicative p t <>
  emitContLikeMonad p t <>
  emitContToDirect p t <>
  emitDirectToCont p t

nameOfClass :: Polynomial -> String
nameOfClass p = "C" ++ showShortPolynomial "x" p

makeFunDeps :: Theory -> [(Name, Name)]
makeFunDeps t = do k <- thKinds t
                   [(thName t, k), (k, thName t)]

hOps :: Theory -> Emitter
hOps t = aux (thOps t)
 where
  aux [] = emit ""
  aux ((Operation _ n [] c) : os) = emitFunType n (emit c) <> newline
                                                           <> aux os
  aux ((Operation _ n d c) : os) =
    emitFunType n (emitArrow (dom d) (emit c)) <> newline <> aux os
  dom (k : t@(_ : ks)) = emitArrow (emit k) (dom t)
  dom [k] = emit k

printHaskell :: HaskellEmitMode -> Polynomial -> Theory -> String
printHaskell mode p t = emitterToString (emitHaskell mode p t)

moduleDesc :: Polynomial -> Theory -> Emitter
moduleDesc p t =
--  emit ">   /oo\\___nn" <> newline <>
--  emit "> ~~~~~~~~~~~~~" <> newline <> newline <>
  --emit "This module was generated by the 'CayMon' tool." <> newline <>
  "Module: " <> nameOfClass p <> newline <>
  "Description: Algebraic theory represented by " <> rep <> newline <>
  "Stability: experimental" <> newline <>
  newline <>
  emit "This module defines the equational theory Cayley-represented by the type"
  <> newline <> newline <> 
  emit "> " <=> rep <>
  newline <> newline
 where
  rep = emit (showLongPolynomial "x" p) <=> emit "->" <=> "x"

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

emitEquations :: HaskellEmitMode -> Theory -> Emitter
emitEquations mode t =
  haddockBlockComment ("Instances should satisfy the following laws:" <>
                       newline <> renderEqs mode (thEqs t))
 where
  renderEqs _    []     = emit ""
  renderEqs Docs (e:es) = emit "\n *  @" <> emit (renderEquation True e) <> emit "@" <> newline <> renderEqs Docs es
  renderEqs Code (e:es) = emit (renderEquation False e) <> newline <> renderEqs Code es

-- emit datatypes

emitDatas :: [AlgebraicDatatype] -> Emitter
emitDatas [] = emit ""
emitDatas (d:ds) = emitData d <> newline <> emitDatas ds

emitDatasDeriving :: [AlgebraicDatatype] -> [String] -> Emitter
emitDatasDeriving []     cl = emit ""
emitDatasDeriving (d:ds) cl = emitData d <> newline <>
  emit "  deriving (" <> emitStringList cl ", " <> emit ")" <>
  newline <> newline <>
  emitDatasDeriving ds cl

emitData :: AlgebraicDatatype -> Emitter
emitData adt@(AlgebraicDatatype name vars cons) =
  emit "data" <=> emit name <=> emitVars vars <=> emit "="
              <=> emitConstructors adt cons
 where
  emitVars []     = emit ""
  emitVars (v:vs) = emit v <=> emitVars vs

emitConstructors :: AlgebraicDatatype -> [Constructor] -> Emitter
emitConstructors adt ([c]) = emitConstructor c
emitConstructors adt (c:cs) =
  emitConstructor c <> newline <> emit (replicate dataHeadLength ' ') <> emit "|" <=>
  emitConstructors adt cs
 where
  dataHeadLength =
    length "data" + 1 + length (adtName adt) + 1 + sum (map length $ typeVars adt) +
    length (typeVars adt)
emitConstructors adt []     = emit ""

emitConstructor :: Constructor -> Emitter
emitConstructor (Constructor name ts) =
  emit name <=> (emitTypes ts)

emitTypes :: [Type] -> Emitter
emitTypes []     = emit ""
emitTypes (t@(TypeConsApp _ (_:_)):ts) = emit "(" <> emitType t <> emit ")" <=> emitTypes ts
emitTypes (t@(TypeArr _ _):ts) = emit "(" <> emitType t <> emit ")" <=> emitTypes ts
emitTypes (t@(TypeForall _ _):ts) = emit "(" <> emitType t <> emit ")" <=> emitTypes ts
emitTypes (t:ts) = emitType t <=> emitTypes ts

emitType :: Type -> Emitter
emitType (TypeVar x) = emit x
emitType (TypeConsApp name ts) = emit name <=> emitTypes ts
emitType (TypeArr t1@(TypeArr _ _) t2) =
  emit "(" <> emitType t1 <> emit ")" <=> emit "->" <=> emitType t2
emitType (TypeArr t1 t2) = emitType t1 <=> "->" <=> emitType t2
emitType (TypeForall x t) =
  emit "forall" <=> emit x <> emit "." <=> emitType t


-- emit direct instance

emitDirectInstanceHeader :: Polynomial -> Theory -> Emitter
emitDirectInstanceHeader p t = instanceHeader (nameOfClass p) args
 where
  args = tcon (upFirst $ thName t) : map (tcon . upFirst) (thKinds t)
  tcon a = "(" <> a <=> varName <> ")"
  varName = "v"

eqToDef :: Equation -> Emitter
eqToDef e =
  (renderTerm False $ haskL $ eqL e) <=> emit "=" <=>
    (renderTerm False $ haskR $ eqR e)
 where
  termUpFirst (TOp f xs) = TOp (upFirst f) (map termUpFirst xs)
  termUpFirst (t@(TVar x)) = t
  haskL (t@(TVar x)) = t
  haskL (TOp f xs) = TOp f (map termUpFirst xs)
  haskR (t@(TVar x)) = t
  haskR (TOp f xs) = TOp (upFirst f) xs

opToDef :: String -> Emitter
opToDef s =
  renderTerm False (TOp s []) <=> emit "=" <=>
    renderTerm False (TOp (upFirst s) [])

emitDirectInstance :: Polynomial -> Theory -> Emitter
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

emitDirectMonadHeader :: Polynomial -> Theory -> Emitter
emitDirectMonadHeader p t =
  instanceHeader "Monad" [upFirst $ thName t]

emitDirectApplicativeHeader :: Polynomial -> Theory -> Emitter
emitDirectApplicativeHeader p t =
  instanceHeader "Applicative" [upFirst $ thName t]

emitApplicativeFromMonad :: Emitter
emitApplicativeFromMonad =
  emit "  pure  = return" <> newline <>
  emit "  (<*>) = ap" <> newline

emitDirectMonadReturn :: Polynomial -> Theory -> Emitter
emitDirectMonadReturn p t =
  emit "  return x" <=> emit "=" <=>
  emit (upFirst $ opName $ consOp) <=>
  emit "\n    " <>
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

emitDirectMonadBind :: Polynomial -> Theory -> Emitter
emitDirectMonadBind p t =
  mainBind <> newline <>
  emit "   where" <> newline <>
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

emitDirectMonadBindViaFold :: Polynomial -> Theory -> Emitter
emitDirectMonadBindViaFold p t =
  "  v >>= f =" <=> foldXName p t <=> "f" <=> "v" <> newline

emitKindBind :: Polynomial -> Theory -> String -> Emitter
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

foldXName :: Polynomial -> Theory -> Emitter
foldXName p t = "fold" ++ upFirst (polyName p)

emitFoldX :: Polynomial -> Theory -> Emitter
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
  
-- emit Cayley representation

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

emitSelectors :: Polynomial -> Theory -> Emitter
emitSelectors p t =
  emitStringList
    (map (emitData . selectorDatatype) $ monomials p)
    "\n"

emitMasterSelector :: Polynomial -> Theory -> Emitter
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

emitSelectorInstanceHeader :: Polynomial -> Theory -> Emitter
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

emitSelectorInstance :: Polynomial -> Theory -> Emitter
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

emitToCayley :: Polynomial -> Theory -> Emitter
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

emitFromCayley :: Polynomial -> Theory -> Emitter
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

-- Continuation-like monad

contLikeTypeName p = "CayMon" -- ++ showShortPolynomial "x" p

contLikeComment :: Polynomial -> Emitter
contLikeComment p
  | functor (monomials p) =
     "Note that in " <> masterSelectorName <> " v the variable v" <=>
     "does not occur on the left-hand side of ->, which means that\n" <>
     masterSelectorName <> " is a functor. Hence, " <> contLikeTypeName p <>
     " is actually a codensity monad."
  | otherwise =
     "Note that in " <> masterSelectorName <> " v the variable v" <=>
     "occurs on the left-hand side of ->, which means that\n" <>
     masterSelectorName <> " is *not* a functor. Hence, " <> contLikeTypeName p <>
     " is *not* a codensity monad."
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

emitContLikeData :: Polynomial -> Theory -> Emitter
emitContLikeData p t = (emitData $ contLikeData p t) <> newline <> newline

emitContLikeFunctor :: Polynomial -> Theory -> Emitter
emitContLikeFunctor p t = instanceHeader "Functor" [contLikeTypeName p] <>
  "  fmap f (" <> contLikeTypeName p <=> "m)" <=> "=" <=> contLikeTypeName p <=> "(\\k -> m (\\x -> k (f x)))" <> newline <> newline

emitContLikeApplicative :: Polynomial -> Theory -> Emitter
emitContLikeApplicative p t = instanceHeader "Applicative" [contLikeTypeName p] <>
  emit "  pure  = return" <> newline <>
  emit "  (<*>) = ap" <> newline <> newline

emitContLikeMonad :: Polynomial -> Theory -> Emitter
emitContLikeMonad p t = instanceHeader "Monad" [contLikeTypeName p] <>
  emit "  return x =" <=> contLikeTypeName p <=> "(\\k -> k x)" <> newline <>
  emit "  m >>= k  =" <=> contLikeTypeName p <=> "(\\c -> run m (\\a -> run (k a) c))" <> newline <>
  emit "   where" <> newline <>
  emit "    run" <=> "(" <> contLikeTypeName p <=> "m) = m" <> newline <> newline

emitContToDirect p t =
  emit "contToDirect ::" <=> contLikeTypeName p <=>  "v" <=> "->" <=>
    upFirst (polyName p) <=> "v" <> newline <>
  emit "contToDirect (" <> contLikeTypeName p <=>
    "m) = fromCayley (m (\\v -> toCayley (return v)))" <> newline <> newline

emitDirectToCont p t =
  emit "directToCont ::" <=> upFirst (polyName p) <=> "v" <=> "->" <=>
    contLikeTypeName p <=>  "v" <> newline <>
  emit "directToCont x" <=> "=" <=> contLikeTypeName p <=> "(\\k ->" <=>
    "fold" <> upFirst (polyName p) <=> "k" <=> "x" <> ")" <> newline <> newline
