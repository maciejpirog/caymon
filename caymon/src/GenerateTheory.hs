module GenerateTheory where

import Theory
import Polynomial

import Data.List (nub)

consName = "cons"
piName i m = "out" ++ show i
epsilonName i m = "unit" ++ "_" ++ kindName m ++ "_" ++ show i
gammaName m n = "comp" ++ "_" ++ kindName m ++ "_" ++ kindName n

theoryFromPolynomial :: Polynomial -> Theory
theoryFromPolynomial (Polynomial name monomials) =
  Theory name kinds operations equations
 where

  -- kinds

  expandedMonomials = do m <- monomials
                         replicate (coeff m) m

  numberedMonomials = zip [1..] expandedMonomials
  
  kindList = map kindName expandedMonomials

  kinds = nub kindList

  -- operations

  operations = cons ++ pis ++ epsilons ++ gammas
  
  cons = [Operation OTCons consName kindList name]

  pis = map (\(i,m) -> Operation OTPi (piName i m) [name] (kindName m))
            numberedMonomials

  epsilons = do m <- monomials
                i <- [1 .. (power m)]
                return $ Operation OTEpsilon (epsilonName i m) [] (kindName m)

  gammas = do m <- monomials
              n <- monomials
              return $
                Operation OTGamma
                  (gammaName m n)
                  (kindName n : replicate (power n) (kindName m))
                  (kindName m)

  -- equations

  equations = cons_pi ++ pi_cons ++ gamma_epsilon_x ++ gamma_x_epsilon
                ++ gamma_gamma

  varx    = "x"
  varxi i = "x" ++ show i
  vary    = "y"
  varyi i = "y" ++ show i
  varz    = "z"
  varzi i = "z" ++ show i

  cons_pi = [Equation EqConsPi
    (TOp
      consName
      (map (\(j,n) -> TOp (piName j n) [TVar varx]) numberedMonomials))
    (TVar varx)]

  pi_cons = do
    (i,m) <- numberedMonomials
    return $ Equation EqPiCons
      (TOp
        (piName i m)
        [TOp consName (map (TVar . varxi . fst) numberedMonomials)])
      (TVar $ varxi i)

  gamma_epsilon_x = do
    m <- monomials
    n <- monomials
    k <- [1 .. (power n)]
    return $ Equation EqGammaEpsilonX
      (TOp
        (gammaName m n)
        (TOp (epsilonName k n) [] : map (TVar . varxi) [1 .. (power n)]))
      (TVar (varxi k))

  gamma_x_epsilon = do
    m <- monomials
    return $ Equation EqGammaXEpsilon
      (TOp
        (gammaName m m)
        (TVar varx : map (\i -> TOp (epsilonName i m) []) [1 .. (power m)]))
      (TVar varx)

  gamma_gamma = do
    m <- monomials
    n <- monomials
    k <- monomials
    return $ Equation EqGammaGamma
      (TOp
        (gammaName m n)
        (TOp
          (gammaName n k)
          (TVar varx : map (TVar . varyi) [1 .. (power k)])
            : map (TVar . varzi) [1 .. (power n)]))
      (TOp
        (gammaName m k)
        (TVar varx
          : map (\i -> TOp (gammaName m n)
                           (TVar (varyi i)
                             : map (TVar . varzi) [1 .. (power n)]))
                [1 .. (power k)]))
