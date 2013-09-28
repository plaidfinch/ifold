module IndentingFold where

import Control.Arrow
import Data.List.Split

let ws = split (keepDelimsL $ oneOf " ") s
in  (concatMap fst *** concatMap fst) .
       span ((< 10) . snd) $
          zip ws (scanl1 (+) (map length ws))
