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
  print $ map' (+ 1) [1, 2, 3]
  print $ map' toUpper "abc"
  print $ words' q

q :: String
q = "To be, or not to be: that is the question."

-- |
-- "To be, or not to be: that is the question." の単語の先頭だけをすべて大文字にせよ
no1 :: String
no1 = unwords [ toUpper c : cs | (c : cs) <- words q ]

-- |
-- "To be, or not to be: that is the question." の記号の後ろに改行を入れよ
no2 :: String
no2 = foldr f "" q
 where
  f cur acc
    | isSpace cur = cur : acc
    | isAlphaNum cur = cur : acc
    | otherwise = case acc of
      (' ' : cs) -> cur : '\n' : cs
      _          -> cur : '\n' : acc

-- |
-- "To be, or not to be: that is the question." の偶数番目の単語はすべて小文字、それ以外は大文字にせよ。ただし To は 0 番目とする
no3 :: String
no3 = unwords $ foldr f [] $ zip [0, 1 ..] $ words q
 where
  f (n, s) acc | even n    = map toLower s : acc
               | otherwise = map toUpper s : acc

-- |
-- map と同じ動作をする関数 map' を定義せよ
map' :: (a -> b) -> [a] -> [b]
map' f ts = go ts []
 where
  go []       acc = reverse acc
  go (x : xs) acc = go xs $ f x : acc

-- |
-- words と同じ動作をする関数 words' を定義せよ
words' :: String -> [String]
words' s = f' : s'
 where
  (f', s') = foldr f ("", []) s
  f cur (w, acc) | isSpace cur = ("", w : acc)
                 | otherwise   = (cur : w, acc)
