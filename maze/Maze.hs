import System.Random (randomRIO)

type Loc = (Int, Int)

type Maze a = Loc -> Status

data Complete
data Incomplete

data Status = Unvisited | Visited Int
  deriving (Show)

initialMaze :: Maze Incomplete
initialMaze = const Unvisited

height = 200
width  = 200

inRange1 :: Int -> Int -> Int -> Bool
inRange1 min max i = i >= min && i <= max

inRange2 :: Loc -> Bool
inRange2 (x,y) = inRange1 0 width x && inRange1 0 height y

neighbors :: Loc -> [Loc]
neighbors (x,y) = filter inRange2 neighbors
  where
    neighbors = [(x+1,y), (x-1,y), (x,y+1), (x,y-1)]

grid :: [[Loc]]
grid = map makeRow [0..height-1]
  where
    makeRow i = [(x,i) | x <- [0..width - 1]]

visit :: Maze Incomplete -> Loc -> Int -> Maze Incomplete
visit maze loc i = \l -> if l == loc then Visited i else maze l

outer :: Maze Incomplete -> Loc -> Int -> IO (Maze Incomplete, Int)
outer maze loc step = do
  let maze' = visit maze loc step
      uvs   = [uv | uv <- (neighbors loc), Unvisited <- [maze uv]]
  inner maze' uvs step
 where
   inner maze []  i = pure (maze, i)
   inner maze uvs i = do
     next       <- (uvs!!) <$> randomRIO (0, length uvs - 1)
     (maze', j) <- outer maze next (i + 1)
     let uvs'    = [uv | uv <- uvs, Unvisited <- [maze' uv]]
     inner maze' uvs' j

validate :: Maze Incomplete -> Maybe (Maze Complete)
validate m
  | null [() | Unvisited <- map m (concat grid) ] = Just m
  | otherwise                                     = Nothing

makePGM :: Maze Complete -> Int -> String
makePGM maze iters = unlines ["P2", show width ++ " " ++ show height, show iters]
                  ++ unlines (map displayRow grid)
  where
    displayRow xs = unwords (map show [ i | Visited i <- map maze xs])

main = do
  [x,y]  <- sequence [randomRIO (0,width-1), randomRIO (0, height-1)]
  (m, i) <- loop initialMaze (x,y) 0
  case validate m of
    Just m' -> writeFile "splotch.pgm" (makePGM m' i)
    Nothing -> putStrLn "Error: There is a bug in the algorithm, not all cells were visited"
