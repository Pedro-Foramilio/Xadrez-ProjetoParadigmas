module Lib ( someFunc ) where

import TiposBase

someFunc :: IO ()
someFunc = do
   print $ show $ validaMovimento (Bishop White) (Position 'A' 1)  (Position 'H' 8)

