module Player where
   
import Data.Text.Internal.Unsafe
import System.Random
import Brillo.Interface.Environment

data Player = AI Direction Pos | Human Direction Pos
  deriving (Eq,Show)

data Direction = UP | DOWN  | LEFT  | RIGHT
  deriving (Eq,Show)
type Pos = (Float,Float)  


move :: Direction -> Pos -> Pos 
move UP (x, y) = (x, y+1)
move DOWN (x, y) = (x, y-1)
move RIGHT  (x, y) = (x+1, y)
move LEFT  (x, y) = (x-1, y)

aiMove :: Player -> State -> (Direction, Pos)
aiMove ai@(AI d p) s = inlinePerformIO ( do
  turn <- turnChance
  if turn || willHitSomething ai s
    then do
      AI newDir _ <- changeDirection ai s
      return (newDir, move newDir p)
    else return (d, move d p))
aiMove (Human d p) _ = (d, move d p)



changeDirection :: Player -> State -> IO Player
changeDirection (AI d p) s = do
  gen <- newStdGen
  let (d1, d2) = directionOptions d
      options = filter (\dir -> not (willHitSomething (AI dir p) s)) [d1, d2]
  case options of
    []      -> return (AI d p)            
    [only]  -> return (AI only p)         
    [x, y]  -> return (AI (randomTuple gen (x, y)) p)
    _ -> return (AI d p)
changeDirection (Human _ _) _ = undefined

randomTuple :: RandomGen g => g -> (Direction, Direction) -> Direction
randomTuple gen (x, y) =
    let (n, _) = randomR (0 :: Int , 1:: Int) gen
    in if n == 0 then x else y
  

directionOptions :: Direction -> (Direction,Direction)
directionOptions UP = (LEFT,RIGHT)
directionOptions LEFT = (UP,DOWN)
directionOptions DOWN = (RIGHT,LEFT)
directionOptions RIGHT = (DOWN,UP)

willHitSomething :: Player -> State -> Bool
willHitSomething player s = case player of
                              (Human d p) -> go d p
                              (AI d p ) -> go d p
  where
    go d p =
      let newPos = move d p
      in newPos `elem` (visited0 s) || newPos `elem` (visited1 s) || outOfBounds newPos


turnChance :: IO Bool
turnChance =   do
  gen <- newStdGen
  let (n, _) = randomR (1::Int, 50::Int) gen 
  return (n == 1)

screenSize::  (Int,Int)
screenSize = inlinePerformIO getScreenSize

outOfBounds :: Pos -> Bool
outOfBounds (x, y) =
  let (w, h) = screenSize
      halfW = fromIntegral w / 2
      halfH = fromIntegral h / 2
  in x < (-halfW) || x > halfW || y < (-halfH) || y > halfH


data State = MkState
  { 
    visited0 :: [Pos], 
    players  :: [Player], 
    visited1 :: [Pos], 
    gameOver :: Bool
  } deriving (Eq, Show)

resetPlayers :: [Player] -> [Player]
resetPlayers [AI _ _, AI _ _] = [AI RIGHT (-200,0),AI LEFT (200,0)]
resetPlayers [Human _ _, Human _ _] = [Human RIGHT (-200,0),Human LEFT (200,0)]
resetPlayers [AI _ _, Human _ _] = [AI RIGHT (-200,0), Human LEFT (200,0)]
resetPlayers _ = [] 


initState :: State
initState = MkState
            []
            [AI RIGHT (-200,0), Human LEFT (200,0)]
            []
            False

initState2Human :: State
initState2Human = MkState
            []
            [Human RIGHT (-200,0),Human LEFT (200,0)]
            []
            False

initState2AI :: State
initState2AI = MkState
            []
            [AI RIGHT (-200,0),AI LEFT (200,0)]
            []
            False

gameMode :: Int -> State
gameMode x 
  | x == 0 = initState2AI
  | x == 1 = initState
  |otherwise = initState2Human