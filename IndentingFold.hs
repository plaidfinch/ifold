module IndentingFold where

import Control.Applicative
import Control.Arrow
import Data.Maybe
import Data.List.Split
import Data.Char

charSize :: Int -> Char -> Int
charSize tabSize x = case x of
   '\t'      -> tabSize
   otherwise -> 1

stringSize :: Int -> String -> Int
stringSize tabSize = sum . map (charSize tabSize)

spanSize :: (a -> Int) -> Int -> [a] -> ([a],[a])
spanSize size len list =
   (map fst *** map fst) .
   span ((<= len) . snd) $
   zip list $ scanl1 (+) (map size list)

splitString :: Int -> Int -> String -> [String]
splitString tabSize maxSize string =
   let (str,rest) = splitChunk string
   in case (str,rest) of
      ("","")     -> []
      ("",rest)   -> let (str',rest') = spanSize (charSize tabSize) maxSize rest
                     in                str' : thisSplit rest'
      otherwise   -> dropWhile isSpace str  : thisSplit rest
   where
      thisSplit = splitString tabSize maxSize
      splitChunk str =
         (concat *** concat) $ spanSize (stringSize tabSize) maxSize ws
         where ws = split (keepDelimsL $ whenElt isSpace) str

-- Takes a line and folds it into a list of lines
foldLine :: Int -> Int -> String -> [String]
foldLine tabSize maxSize line =
   if dropWhile isSpace line == "" then [""] -- if line is only spaces, print a newline
   else if (stringSize tabSize) tabs < maxSize
      then map (tabs ++) $ splitString tabSize maxSize' string
      else                 splitString tabSize maxSize  string
   where (tabs,string) = span isSpace line
         maxTabs = floor (toRational maxSize / toRational tabSize)
         maxSize' = maxSize - (stringSize tabSize) tabs

