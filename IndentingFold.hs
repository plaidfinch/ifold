module IndentingFold where

import Control.Applicative
import Control.Arrow
import Data.Maybe
import Data.List.Split

tabSize :: Int
tabSize = 8

letterSize :: Char -> Int
letterSize x = if x == '\t' then tabSize else 1

stringSize :: String -> Int
stringSize =
   sum . map letterSize

splitString :: (String -> Int) -> Int -> String -> [String]
splitString size maxSize string =
   let (str,rest) = splitChunk string
   in case (str,rest) of
      ("","")     -> []
      (str,"")    -> [str]
      ("",rest)   -> take maxSize rest : splitString size maxSize (drop maxSize rest)
      (' ':str,_) -> str : splitString size maxSize rest
      otherwise   -> str : splitString size maxSize rest
   where
      splitChunk str =
         (concatMap fst *** concatMap fst) .
            span ((<= maxSize) . snd) $
               zip ws (scanl1 (+) (map size ws))
         where ws = split (dropInitBlank $ oneOf " ") str

-- Takes a line and folds it into a list of lines, returning Nothing in the case where there is a wider initial segment of tabs than the line (we can't ever give a good formatting for this case).
foldLine :: (String -> Int) -> Int -> String -> Maybe [String]
foldLine size maxSize line =
   if size tabs < maxSize
      then Just $ map (tabs ++) $ splitString size (maxSize - size tabs) string
      else Nothing
   where (tabs,string) = span whitespace line
         whitespace = any id . (map (==) "\t " <*>) . (:[])

