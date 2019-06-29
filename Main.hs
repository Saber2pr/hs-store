{- by saber2pr 2019.6.28 11:59 -}
module Main where

data State = State {
  count :: Integer
} deriving (Show)

class Store s where
  dispatch :: s -> IO ()
  increase :: Integer -> s -> s

instance Store State where
  dispatch = print.count
  increase n s = State $ (+n).count $ s

main :: IO ()
main = dispatch $ (reducer.reducer.reducer) initialState -- 3
  where
    initialState = State 0
    reducer = increase 1
