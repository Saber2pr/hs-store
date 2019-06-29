{- by saber2pr 2019.6.28 11:59 -}
module Main where

data State = State {
    count   :: Integer
  , message :: String
} deriving (Show)

data Action = Action {
    name       :: String
  , payloadStr :: String
  , payloadInt :: Integer
} deriving (Show)

class Store s where
  dispatch :: Monad m => s -> (s -> m State) -> m s
  increase :: Integer -> s -> s
  update   :: String -> s -> s
  reducer  :: s -> Action -> s

instance Store State where
  dispatch s f = f s
  increase n s = State ((+n).count $ s) $ message s
  update   m s = State (count s) m
  reducer s a  = case name a of
    "add" -> increase (payloadInt a) s
    "set" -> update (payloadStr a) s
    _     -> s

main :: IO ()
main = do
  next <- dispatch (reducer initialState action_add) listener
  res <- dispatch (reducer next action_set) listener
  return ()
  where
    initialState = State 0 ""
    action_add       = Action "add" "" 2
    action_set       = Action "set" "hello redux" 0
    listener  s  = do
      print s
      return s
