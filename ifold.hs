module Main where

import Pipes
import qualified Pipes.Prelude as P

import Data.Maybe

import System.IO
import System.Console.GetOpt
import System.Environment
import System.Exit

import IndentingFold

defaultTabSize :: Int
defaultTabSize = 8

defaultLineWidth :: Int
defaultLineWidth = 80

parseWithDefault :: Read c => c -> String -> c
parseWithDefault d =
   fromMaybe d . fmap fst . listToMaybe . reads

main :: IO ()
main = do

   progName <- getProgName
   argv <- getArgs

   let usageMessage = "Usage: " ++ progName ++ " [integer line width] [integer tab size]"
   
   (w,t) <- case argv of
      []        -> return (defaultLineWidth,defaultTabSize)
      [w]       -> return (parseWithDefault defaultLineWidth w,defaultTabSize)
      [w,t]     -> return (parseWithDefault defaultLineWidth w,parseWithDefault defaultTabSize t)
      otherwise -> ioError (userError usageMessage)

   runEffect $ for P.stdinLn (mapM_ yield . foldLine t w) >-> P.stdoutLn
