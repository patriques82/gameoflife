module Lib
    ( run
    , Grid(..)
    , Cell(..)
    , grid
    , tick
    , neighbours
    , hOverflow
    ) where

import qualified Data.Map.Strict as M
import Data.List (intercalate)
import Data.List.Split (chunksOf)
import Control.Monad (join, void)
import Control.Concurrent (threadDelay)

data Grid = Grid { width :: Int
                 , _grid :: M.Map Pos Cell
                 } deriving Eq

instance Show Grid where
  show (Grid w g) =
    intercalate "\n" . chunksOf w . join . fmap show . M.elems $ g

grid :: Int -> [Int] -> Grid
grid w = Grid w . M.fromList . zip [1..] . fmap f
  where f 0 = Dead
        f _ = Live

data Cell = Live | Dead
  deriving Eq

instance Semigroup Cell where
  Live <> Live = Live
  _ <> _       = Dead

instance Show Cell where
  show Live = "x"
  show Dead = " "

type Pos = Int
type Width = Int

run :: Grid -> IO ()
run = void . loop

loop :: Grid -> IO Grid
loop g = do
  display g
  loop (tick g)

display :: Grid -> IO ()
display g = do
  putStrLn (ansiClearScreen ++ ansiGoto 1 1 ++ show g)
  threadDelay 1000000

ansiClearScreen = "\ESC[2J"
ansiGoto x y = "\ESC[" ++ show y ++ ";" ++ show x ++ "H"

-- Logic

tick :: Grid -> Grid
tick g@(Grid w m) = Grid w m'
  where
    m' = M.mapWithKey (conwaysRules g) m

conwaysRules :: Grid -> Pos -> Cell -> Cell
conwaysRules g p c
  | not (isAlive c) && livingNeighbours g p == 3 = Live
  | isAlive c       && livingNeighbours g p < 2  = Dead
  | isAlive c       && livingNeighbours g p > 3  = Dead
  | otherwise                                    = c

livingNeighbours :: Grid -> Pos -> Int
livingNeighbours g = length . filter isAlive . neighbours g

isAlive :: Cell -> Bool
isAlive Live = True
isAlive Dead = False

neighbours :: Grid -> Pos -> [Cell]
neighbours (Grid w m) p = M.elems $ M.intersection m m'
  where
    m' = M.fromList
        . (`zip` (repeat ()))
        . filter (not . hOverflow w p)
        $ potentialNeighbours w p

potentialNeighbours :: Width -> Pos -> [Pos]
potentialNeighbours w p = leftNeighbours w p ++ [p-w, p+w] ++ rightNeighbours w p

leftNeighbours w p = [ p-1-w, p-1, p-1+w ]

rightNeighbours w p = [ p+1-w, p+1, p+1+w ]

hOverflow :: Width -> Pos -> Pos -> Bool
hOverflow w p p' = isAtRightBorder w p && any (== p') (rightNeighbours w p)
                || isAtLeftBorder w p && any (== p') (leftNeighbours w p)

isAtRightBorder w p = p `mod` w == 0

isAtLeftBorder w p = (p - 1) `mod` w == 0
