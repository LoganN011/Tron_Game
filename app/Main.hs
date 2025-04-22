
module Main where
import Brillo
import Brillo.Interface.IO.Game (Event (EventKey), Key (Char))

data Direction = UP | DOWN  | LEFT  | RIGHT 
data Player = AI Pos | Human Direction Pos 
type Pos = (Int,Int)

move :: Direction -> Pos -> Pos
move UP (x, y) = (x, y+1)
move DOWN (x, y) = (x, y-1)
move RIGHT  (x, y) = (x+1, y)
move LEFT  (x, y) = (x-1, y)



size = 500


data State =
  MkState { 
    visted :: [[Bool]], -- maybe make this a list of POS so I can draw the line easier or have both
    players :: [Player],
    grid :: [[Pos]]
      
    } -- deriving(Eq,Show)

initState:: State
initState = MkState
            (replicate 500 (replicate 500 False))
            [AI (-200,-200) ,Human LEFT (200,200)]
            [[]] -- make method to fill in all of the postions based on the size or something similar. 
            -- might not need if I just do the postions visted list thing




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
  60 -- frame rate so maybe lower if it moves too fast or something im not sure
  initState
  stateToPicture
  eventUpdate
  (\ f -> stateToState)
