module Main where

import Polynomial
import Theory
import GenerateTheory
import HaskellEmit

import System.IO
import System.Directory
import qualified System.Environment as Environment
import qualified System.Process as Process
import System.Console.ANSI

-- Things to do with cmd line

data Mode = Help | Version | Haskell | Haddock
  deriving (Show,Eq)

data MinorMode = QuickCheck
  deriving (Show,Eq)

data CmdLine = CmdLine { mode       :: Mode
                       , minorModes :: [MinorMode]
                       , polys      :: [Polynomial]
                       , outDir     :: String
                       }
  deriving(Show)

defCmdLine :: CmdLine
defCmdLine = CmdLine Haskell [] [] ""

parseCmdLine :: [String] -> CmdLine
parseCmdLine [] = CmdLine Help [] [] ""
parseCmdLine xs = aux xs defCmdLine
 where
   aux ("--haskell":xs) c = aux xs $ c {mode = Haskell}
   aux ("--haddock":xs) c = aux xs $ c {mode = Haddock}
   aux ("-h":xs)        c = aux xs $ c {mode = Haskell}
   aux ("-d":xs)        c = aux xs $ c {mode = Haddock}
   aux (v:xs)           c | elem v ["--quickcheck", "--QuickCheck", "-q"]
                          = aux xs $ c {minorModes = QuickCheck : minorModes c}
   aux (v:xs)           c | elem v ["--version", "-V", "-v", "--license", "-version",
                                    "-license"]
                          = aux xs $ c {mode   = Version}
   aux (h:xs)           c | elem h ["-h", "--help", "-?", "-help"]
                          = aux xs $ c {mode   = Help}
   aux ("-o":d:xs)      c = aux xs $ c {outDir = d ++ "/"}
   aux ("--out":d:xs)   c = aux xs $ c {outDir = d ++ "/"}
   aux (o@('-':_):_)    c = error $ "unrecognised option " ++ o
   aux (p:xs)           c = aux xs $ c {polys  = Polynomial.parsePoly p : polys c}
   aux []               c = c

-- Haskell mode

runHaskell :: CmdLine -> IO ()
runHaskell cmd = sequence_ $ map runH $ polys cmd
 where
  runH p = do let t = theoryFromPolynomial p
              let output = printHaskell (cmdToEmitMode $ mode cmd)
                                        (QuickCheck `elem` minorModes cmd) p t
              createDirectoryIfMissing True $ outDir cmd
              outh <- openFile (outDir cmd ++ nameOfClass p ++ ".hs") WriteMode
              hPutStr outh output
              hClose outh

cmdToEmitMode :: Mode -> HaskellEmitMode
cmdToEmitMode Haskell = Code
cmdToEmitMode Haddock = Docs

-- Haddock mode

runHaddock :: CmdLine -> IO ()
runHaddock cmd = do
  runHaskell cmd
  Process.system $ "haddock -w --html --hyperlinked-source" ++
                   " -o " ++ outDir cmd ++ "doc" ++
                   " " ++ outDir cmd ++ "*.hs"
  return ()

-- Version info

runVersion :: CmdLine -> IO ()
runVersion _ = do
  ansiLogo
  putStrLn versionInfo
  euSupport

ansiLogo :: IO ()
ansiLogo = do
  setSGR [SetColor Foreground Vivid Green]
  putStr "\n  /oo\\___nn"
  setSGR [Reset]
  setSGR [SetConsoleIntensity BoldIntensity]
  putStr "      "
  putStr "CAYMON"
  putStr ": A tool to generate equational theories and\n"
  setSGR [Reset]
  setSGR [SetColor Foreground Vivid Blue]
  putStr "~~~~~~~~~~~~~"
  setSGR [Reset]
  setSGR [SetConsoleIntensity BoldIntensity]
  putStr "    monads from Cayley representations\n\n"
  setSGR [Reset]

versionInfo :: String
versionInfo =
 "Version 1.0\n" ++
 "This version generates theories and monads from types of the shape PX->X, where P is a polynomial with natural coefficients.\n" ++
 "\n" ++
 "This software is distributed under the MIT license, see\n" ++
 "https://opensource.org/licenses/MIT\n"

euSupport :: IO ()
euSupport = do
  sequence_ $ map toAnsi $
   "##****## This project has received funding from the European Union’s\n" ++
   "#*####*# Horizon 2020 research and innovation programme under the\n" ++
   "#*####*# Marie Skłodowska-Curie grant agreement No 665778.\n" ++
   "##****## \n\n"
 where
  toAnsi '*' = do setSGR [SetColor Foreground Vivid Yellow
                         ,SetColor Background Dull Blue]
                  putStr "*"
                  setSGR [Reset]
  toAnsi '#' = do setSGR [SetColor Background Dull Blue]
                  putStr " "
                  setSGR [Reset]
  toAnsi c = putStr $ return c

-- Help

runHelp :: CmdLine -> IO ()
runHelp _ = do
  setSGR [SetConsoleIntensity BoldIntensity]
  putStr "Usage:"
  setSGR [Reset]
  putStrLn $
    " caymon [POLYNOMIAL]... [OPTION]...\n\n" ++
    "Polynomials should be given as monomials separated by '+', where each monomial\nis given as c*x^n, where 'c' and 'n' are positive integers, and all of the c*,\nx, and ^n parts are optional (but you have to give at least one). Note that\n\"3^2\" means \"3*x^2\" and not \"9\".\n" ++
    "Examples: \"3*x^2+x^2+x+4\" \"x+x\" \"3*x^2 + x\"\n"
  setSGR [SetConsoleIntensity BoldIntensity]
  putStrLn "Options:"
  setSGR [Reset]
  putStrLn $
    o "-h  --haskell     Haskell mode (default)" ++
    o "-d  --haddock     Haskell mode with more Haddock-friendly comments" ++
    o "                  and automatically run Haddock for documentation" ++
    o "-q, --quickcheck  Add QuickCheck tests to output" ++
    o "-o, --out         Specify the target directory for output files" ++
    o "-h, --help        Show this message" ++
    o "-V, --version     Show version information"
 where
  o x = "  " ++ x ++ "\n"

-- The main function

main :: IO ()
main = do
  args <- Environment.getArgs
  let cmd = parseCmdLine args
  case mode cmd of
    Haskell -> runHaskell cmd
    Haddock -> runHaddock cmd
    Version -> runVersion cmd
    Help    -> runHelp cmd
