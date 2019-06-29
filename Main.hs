{- by saber2pr 2019.6.28 11:59 -}
module Main where

data State = State {
  count :: Integer
} deriving (Show)

data Action = Action {
    name :: String
  , payload :: Integer
}

class Store s where
  dispatch :: s -> IO ()
  increase :: Integer -> s -> s
  reducer :: s -> Action -> s

instance Store State where
  dispatch    = print.count
  increase n  = State.(+n).count
  reducer s a = case name a of
    "add" -> increase (payload a) s
    _     -> s

main :: IO ()
main = dispatch $ (reducer initialState action) -- 2
  where
    initialState = State 0
    action = Action "add" 2
