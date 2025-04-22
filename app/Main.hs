
module Main where
import Brillo
import Brillo.Interface.IO.Game (Event (EventKey), Key (Char))

data Direction = UP | DOWN  | LEFT  | RIGHT 
data Player = AI | Human Direction Pos 
type Pos = (Int,Int)

move :: Direction -> Pos -> Pos
move UP (x, y) = (x, y+1)
move DOWN (x, y) = (x, y-1)
move RIGHT  (x, y) = (x+1, y)
move LEFT  (x, y) = (x-1, y)



size = 500


data State =
  MkState { 
    visted :: [[Bool]],
    players :: [Player],
    grid :: [[Pos]]
      
    } -- deriving(Eq,Show)

initState:: State
initState = undefined


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
