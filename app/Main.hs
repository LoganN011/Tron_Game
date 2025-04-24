
module Main where
import Brillo
import Brillo.Interface.IO.Game (Event (EventKey), Key (Char), KeyState (Up))

data Direction = UP | DOWN  | LEFT  | RIGHT
data Player = AI Pos | Human Direction Pos
type Pos = (Float,Float)

move :: Direction -> Pos -> Pos
move UP (x, y) = (x, y+1)
move DOWN (x, y) = (x, y-1)
move RIGHT  (x, y) = (x+1, y)
move LEFT  (x, y) = (x-1, y)

aiMove :: Direction -> Pos -> Pos
aiMove = move



size = 500


data State =
  MkState {
    --visted :: [[Bool]], -- maybe make this a list of POS so I can draw the line easier or have both
    visited :: [Pos],
    players :: [Player],
    grid :: [[Pos]]

    } -- deriving(Eq,Show)

initState:: State
initState = MkState
            []
            --(replicate 500 (replicate 500 False))
            [Human LEFT (200,200)]
            [[]] -- make method to fill in all of the postions based on the size or something similar. 
            -- might not need if I just do the postions visted list thing




stateToPicture:: State -> Picture
stateToPicture s =pictures [ pictures (go (players s)), color white (line (visited s))]
  where
    go [] = []
    go (Human _ (x,y):rest) = Color white (Polygon [(x + dx, y + dy) | dx <- [-1..1], dy <- [-1..1]]) : go rest
    go (AI (x,y):rest) = Color white (Polygon [(x + dx, y + dy) | dx <- [-1..1], dy <- [-1..1]]) : go rest

stateToState:: State -> State
stateToState s = s { players = newPlayers, visited = visited s ++ newVisited }
 where
    (newPlayers, newVisited) = go (players s)

    go [] = ([], [])
    go (Human d p : rest) =
      let newPos = move d p
          (restPlayers, restVisited) = go rest
      in (Human d newPos : restPlayers, newPos : restVisited)
    go (AI p : rest) =
      let newPos = aiMove RIGHT p
          (restPlayers, restVisited) = go rest
      in (AI newPos : restPlayers, newPos : restVisited)


-- stateToState s = s {players = go (players s)}
--   where
--     go [] = []
--     go (Human d p:rest) = Human d (move d p) : go rest -- Find some way of adding the postion to the visted list once it is moved it 
--     go (AI p:rest) = AI (aiMove RIGHT p) :rest  -- make do computer movement method 

eventUpdate::Event-> State -> State
eventUpdate (EventKey (Char c) _ _ _) s = case c of
                                            'w' -> s { players = updateHumanDirection UP s} 
                                            'a' -> s { players = updateHumanDirection LEFT s} 
                                            's' -> s { players = updateHumanDirection DOWN s} 
                                            'd' -> s { players = updateHumanDirection RIGHT s} 
                                            _ -> initState
eventUpdate _ s = initState


updateHumanDirection :: Direction-> State  -> [Player]
updateHumanDirection d s = foldr go [] (players s)
  where
    go (Human _ p) recur = Human d p : recur
    go ai recur = ai : recur



main :: IO ()
main = play FullScreen--(InWindow "Tron" (100,100) (size,size))
  black
  60 -- frame rate so maybe lower if it moves too fast or something im not sure
  initState
  stateToPicture
  eventUpdate
  (\f -> stateToState)
