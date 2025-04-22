
module Main where
import Brillo
import Brillo.Interface.IO.Game (Event (EventKey), Key (Char))

data State =
  MkState { 
      
    }deriving(Eq,Show)

initState:: State
initState = MkState 


stateToPicture:: State -> Picture
stateToPicture s = undefined

stateToState:: State -> State
stateToState s = undefined

eventUpdate::Event-> State -> State 
eventUpdate (EventKey (Char c) _ _ _) s = case c of 
                                            'w' -> initState
                                            _ -> initState
                                            
eventUpdate _ s = initState



main :: IO ()
main = play FullScreen
  black
  60
  initState
  stateToPicture
  eventUpdate
  (\ f -> stateToState)
