module Main where

import Pipes
import qualified Pipes.Prelude as P

import Data.Maybe
import Data.Either

import System.IO

import IndentingFold

main :: IO ()
main = runEffect $
   for P.stdinLn (mapM_ yield . foldLine 20) >-> P.stdoutLn
