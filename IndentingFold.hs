module IndentingFold where

import Control.Applicative
import Control.Arrow
import Data.Maybe
import Data.List.Split

spaceChars :: [Char]
spaceChars = "\t "

whitespace :: Char -> Bool
whitespace = any id . (map (==) spaceChars <*>) . (:[])

tabSize :: Int
tabSize = 8

charSize :: Char -> Int
charSize x = case x of
   '\t'      -> tabSize
   otherwise -> 1

stringSize :: String -> Int
stringSize = sum . map charSize

spanSize :: (a -> Int) -> Int -> [a] -> ([a],[a])
spanSize size len list =
   (map fst *** map fst) .
   span ((<= len) . snd) $
   zip list $ scanl1 (+) (map size list)

splitString :: Int -> String -> [String]
splitString maxSize string =
   let (str,rest) = splitChunk string
   in case (str,rest) of
      ("","")     -> []
      ("",rest)   -> let (str',rest') = spanSize charSize maxSize rest
                     in                   str' : thisSplit rest'
      otherwise   -> dropWhile whitespace str  : thisSplit rest
   where
      thisSplit = splitString maxSize
      splitChunk str =
         (concat *** concat) $ spanSize stringSize maxSize ws
         where ws = split (keepDelimsL $ oneOf spaceChars) str

-- Takes a line and folds it into a list of lines, returning Nothing in the case where there is a wider initial segment of tabs than the line (we can't ever give a good formatting for this case).
foldLine :: Int -> String -> [String]
foldLine maxSize line =
   if stringSize tabs < maxSize
      then map (tabs ++) $
           splitString (maxSize - stringSize tabs) string
      else splitString  maxSize                    string
   where (tabs,string) = span whitespace line
         maxTabs = floor (toRational maxSize / toRational tabSize)

