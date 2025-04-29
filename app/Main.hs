{-# LANGUAGE LambdaCase #-}

module Main where
import Brillo
import Brillo.Interface.IO.Game 

import Player


 
stateToPicture:: State -> Picture
stateToPicture s 
  |gameOver s =  Translate (-350) 0 (Color red (Text "Game Over"))
  |otherwise =pictures [ pictures (go (players s)), color white (line (visitedHuman s)),color orange (line (visitedAI s)) ]
  where
    go [] = []
    go (Human _ (x,y):rest) = Color white (Polygon [(x + dx, y + dy) | dx <- [-2..2], dy <- [-2..2]]) : go rest
    go (AI _ (x,y):rest) = Color orange (Polygon [(x + dx, y + dy) | dx <- [-1..1], dy <- [-1..1]]) : go rest


stateToState :: State -> State
stateToState s
  | gameOver s = s 
  | otherwise = 
      let (newPlayers, newVisitedHuman, newVisitedAI) = go (players s)
          allVisited = visitedHuman s ++ visitedAI s
          newPositions = map (\case
                                Human _ p -> p
                                AI _ p -> p) newPlayers
          collision = any (\pos -> pos`elem` allVisited || outOfBounds pos) newPositions
      in if collision
         then s { gameOver = True } 
         else s { players = newPlayers,
                  visitedHuman = visitedHuman s ++ newVisitedHuman,
                  visitedAI = visitedAI s ++ newVisitedAI }
  where
    go [] = ([], [], [])
    go (Human d p : rest) =
      let newPos = move d p
          (restPlayers, restHuman, restAI) = go rest
      in (Human d newPos : restPlayers, newPos : restHuman, restAI)

    go (ai@(AI d p) : rest) =
      let (dir,newPos) = aiMove ai s
          (restPlayers, restHuman, restAI) = go rest
      in (AI dir newPos : restPlayers, restHuman, newPos : restAI)

 
eventUpdate::Event-> State -> State
eventUpdate (EventKey (SpecialKey KeySpace) _ _ _) s = if gameOver s then initState else s
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

updateAiDirection :: Direction-> State  -> [Player]
updateAiDirection d s = foldr go [] (players s)
  where
    go (AI _ p) recur = AI d p : recur
    go human recur = human : recur

main :: IO ()
main = play FullScreen
  black
  60 -- frame rate so maybe lower if it moves too fast or something im not sure
  initState
  stateToPicture
  eventUpdate
  (\f -> stateToState)
