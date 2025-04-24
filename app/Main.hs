
module Main where
import Brillo
import Brillo.Interface.IO.Game (Event (EventKey), Key (Char), KeyState (Up))

data Direction = UP | DOWN  | LEFT  | RIGHT
  deriving (Eq,Show)
data Player = AI Direction Pos | Human Direction Pos
  deriving (Eq,Show)
type Pos = (Float,Float)

move :: Direction -> Pos -> Pos
move UP (x, y) = (x, y+1)
move DOWN (x, y) = (x, y-1)
move RIGHT  (x, y) = (x+1, y)
move LEFT  (x, y) = (x-1, y)

aiMove :: Direction -> Pos -> Pos
aiMove = move


willHitSomething :: Player -> State -> Bool

willHitSomething player s = case player of 
                              (Human d p) -> go d p
                              (AI d p ) -> go d p 
  where 
    go d p = 
      let newPos = move d p 
      in newPos `elem` (visitedAI s) || newPos `elem` (visitedHuman s)
-- need to update to check if we are going off the screen somehow 


data State =
  MkState {
    visitedHuman :: [Pos], -- need to convert this to a 2d list 
    players :: [Player],
    visitedAI :: [Pos]

    } deriving(Eq,Show)

initState:: State
initState = MkState
            []
            --[Human LEFT (200,200)]
            [AI RIGHT (-200,200), Human LEFT (200,200)]
            []  
            

stateToPicture:: State -> Picture
stateToPicture s =pictures [ pictures (go (players s)), color white (line (visitedHuman s)),color orange (line (visitedAI s)) ]
  where
    go [] = []
    go (Human _ (x,y):rest) = Color white (Polygon [(x + dx, y + dy) | dx <- [-1..1], dy <- [-1..1]]) : go rest
    go (AI _ (x,y):rest) = Color orange (Polygon [(x + dx, y + dy) | dx <- [-1..1], dy <- [-1..1]]) : go rest

stateToState:: State -> State
stateToState s = s 
  { players = newPlayers, visitedHuman = visitedHuman s ++ newVisitedHuman, visitedAI = visitedAI s ++ newVisitedAI }
  where
    (newPlayers, newVisitedHuman, newVisitedAI) = go (players s)

    go [] = ([], [], [])
    go (Human d p : rest) =
      let newPos = move d p
          (restPlayers, restHuman, restAI) = go rest
      in (Human d newPos : restPlayers, newPos : restHuman, restAI)

    go (AI d p : rest) =
      let newPos = aiMove RIGHT p
          (restPlayers, restHuman, restAI) = go rest
      in (AI d newPos : restPlayers, restHuman, newPos : restAI)


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
                                            _ -> s
eventUpdate _ s = s


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
