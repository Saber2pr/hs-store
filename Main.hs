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

instance Store State where
  createStore l a s  = do
    l nextState
    return nextState
    where
      nextState = reducer s a

  reducer s a  = case name a of
    "add" -> increase (payloadInt a) s
    "set" -> update (payloadStr a) s
    _     -> s

  increase n s = State ((+n) $ count s) $ message s
  update   m s = State (count s) m

class Store s where
  createStore :: Monad m => (s -> m ()) -> Action -> s -> m s
  reducer  :: s -> Action -> s

  increase :: Integer -> s -> s
  update   :: String -> s -> s

main :: IO State
main = dispatch action_add initialState
   >>= dispatch action_set
   >>= dispatch action_add
   >>= dispatch action_add
   >>= dispatch action_add
  where
    initialState = State 0 ""

    action_add   = Action "add" ""            2
    action_set   = Action "set" "hello redux" 0

    listener     = print
    dispatch     = createStore listener
