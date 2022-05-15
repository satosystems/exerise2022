module Lib
  ( someFunc
  ) where

import           Data.Char                      ( toUpper )

someFunc :: IO ()
someFunc = do
  putStrLn no1

q :: String
q = "To be, or not to be: that is the question."

no1 :: String
no1 = unwords [ toUpper c : cs | (c : cs) <- words q ]
