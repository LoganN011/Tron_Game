
module Main where
import Brillo
import Brillo.Interface.IO.Game 

import Player


stateToPicture :: State -> Picture
stateToPicture s
  | gameOver s = Translate (-350) 0 (Color red (Text "Game Over"))
  | otherwise =
      pictures [ drawPlayers (players s)
               , drawTrail orange (visited0 s)
               , drawTrail blue  (visited1 s) ]

drawTrail :: Color -> [Pos] -> Picture
drawTrail c = pictures . map (\(x, y) -> Translate x y (Color c (ThickCircle 0.5 4)))

--Can remove because it is hard to see when running with the thich line 
drawPlayers :: [Player] -> Picture
drawPlayers ps = pictures (zipWith drawPlayer [0..] ps)
  where
    drawPlayer 0 (Human _ (x, y)) = Color orange (Polygon [(x + dx, y + dy) | dx <- [-2..2], dy <- [-2..2]])
    drawPlayer 0 (AI _ (x, y))    = Color orange (Polygon [(x + dx, y + dy) | dx <- [-2..2], dy <- [-2..2]])
    drawPlayer 1 (Human _ (x, y)) = Color blue  (Polygon [(x + dx, y + dy) | dx <- [-2..2], dy <- [-2..2]])
    drawPlayer 1 (AI _ (x, y))    = Color blue  (Polygon [(x + dx, y + dy) | dx <- [-2..2], dy <- [-2..2]])
    drawPlayer _ _               = Blank



stateToState :: State -> State
stateToState s
  | gameOver s = s
  | otherwise =
      let (newPlayers, newVisited0, newVisited1) = processPlayers (players s)
          allVisited = visited0 s ++ visited1 s
          newPositions = map getPos newPlayers
          collision = any (\pos -> pos `elem` allVisited || outOfBounds pos) newPositions
      in if collision
         then s { gameOver = True }
         else s { players = newPlayers
                , visited0 = visited0 s ++ newVisited0
                , visited1 = visited1 s ++ newVisited1 }

  where
    getPos (Human _ p) = p
    getPos (AI _ p) = p

    processPlayers :: [Player] -> ([Player], [Pos], [Pos])
    processPlayers [p0, p1] =
      let (newP0, trail0) = updatePlayer p0
          (newP1, trail1) = updatePlayer p1
      in ([newP0, newP1], trail0, trail1)
    processPlayers _ = (players s, [], []) 

    updatePlayer :: Player -> (Player, [Pos])
    updatePlayer (Human d pos) =
      let newPos = move d pos
      in (Human d newPos, [newPos])
    updatePlayer ai@(AI _ _) =
      let (dir, newPos) = aiMove ai s
      in (AI dir newPos, [newPos])

 
eventUpdate :: Event -> State -> State
eventUpdate (EventKey (SpecialKey KeySpace) _ _ _) s = if gameOver s then initState else s
eventUpdate (EventKey (Char c) _ _ _) s = case c of
  'w' -> s { players = updateNthPlayerDirection 0 UP s }
  'a' -> s { players = updateNthPlayerDirection 0 LEFT s }
  's' -> s { players = updateNthPlayerDirection 0 DOWN s }
  'd' -> s { players = updateNthPlayerDirection 0 RIGHT s }
  _   -> s
eventUpdate (EventKey (SpecialKey key) _ _ _) s = case key of
  KeyUp    -> s { players = updateNthPlayerDirection 1 UP s }
  KeyLeft  -> s { players = updateNthPlayerDirection 1 LEFT s }
  KeyDown  -> s { players = updateNthPlayerDirection 1 DOWN s }
  KeyRight -> s { players = updateNthPlayerDirection 1 RIGHT s }
  _        -> s
eventUpdate _ s = s

updateNthPlayerDirection :: Int -> Direction -> State -> [Player]
updateNthPlayerDirection n d s = go n (players s)
  where
    go _ [] = []
    go 0 (Human _ p : rest) = Human d p : rest
    go i (Human dir pos : rest) = Human dir pos : go (i - 1) rest
    go i (ai : rest) = ai : go i rest


-- updateHumanDirection :: Direction-> State  -> [Player]
-- updateHumanDirection d s = foldr go [] (players s)
--   where
--     go (Human _ p) recur = Human d p : recur
--     go ai recur = ai : recur

-- updateAiDirection :: Direction-> State  -> [Player]
-- updateAiDirection d s = foldr go [] (players s)
--   where
--     go (AI _ p) recur = AI d p : recur
--     go human recur = human : recur

-- main :: IO ()
main =
  do
  putStrLn "What speed do you want to play?\n60 is the normal speed"
  input <- getLine
  let speed = read input
  putStrLn "What mode would you like to play?\n0,1, or 2 Players"
  number <- getLine
  let mode = gameMode (read number)
  play FullScreen black speed mode stateToPicture eventUpdate (\f -> stateToState)
