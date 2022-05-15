module Lib
  ( someFunc
  ) where

import           Data.Char                      ( isAlphaNum
                                                , isSpace
                                                , toLower
                                                , toUpper
                                                )

someFunc :: IO ()
someFunc = do
  putStrLn no1
  putStrLn no2
  putStrLn no3

q :: String
q = "To be, or not to be: that is the question."

no1 :: String
no1 = unwords [ toUpper c : cs | (c : cs) <- words q ]

no2 :: String
no2 = foldr f "" q
 where
  f cur acc
    | isSpace cur = cur : acc
    | isAlphaNum cur = cur : acc
    | otherwise = case acc of
      (' ' : cs) -> cur : '\n' : cs
      _          -> cur : '\n' : acc

no3 :: String
no3 = unwords $ foldr f [] $ zip [0, 1 ..] $ words q
 where
  f (n, s) acc | even n    = map toLower s : acc
               | otherwise = map toUpper s : acc
