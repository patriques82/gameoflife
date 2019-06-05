module Lib
    ( run
    , Seed(..)
    ) where

import qualified Data.Map.Strict as M

import Data.List (intercalate)
import Data.List.Split (chunksOf)
import System.Random (randomRs, newStdGen)
import Control.Monad (join, void)
import Control.Concurrent (threadDelay)

-- Types

data Seed = Seed { width :: Int
                 , height :: Int
                 , liveCells :: Int
                 }

data GameState = GameState { gWidth :: Int
                           , grid :: M.Map Pos Cell
                           }

instance Semigroup GameState where
  a <> b = GameState { gWidth = max (gWidth a) (gWidth b)
                     , grid = grid a <> grid b
                     }

instance Show GameState where
  show (GameState w g) =
    intercalate "\n" . chunksOf w . join . fmap show . M.elems $ g

data Cell = Live | Dead

instance Show Cell where
  show Live = "X"
  show Dead = " "

type Pos = Int

-- Run

run :: Seed -> IO ()
run s = void $ initGame s >>= loop

loop :: GameState -> IO GameState
loop g = putStrLn (show g) >> threadDelay 1000000 >> loop (tick g)

initGame :: Seed -> IO GameState
initGame s = do
  g <- newStdGen
  let total = width s * height s
      alive = M.fromList $ take (liveCells s) $ ls
      ls = randomRs (0, total) g `zip` repeat Live
      dead = M.fromList $ take (total - liveCells s) $ ds
      ds = [(p, c) | p <- [1..total], c <- repeat Dead]
  return $ GameState (width s) (alive `M.union` dead)

-- Logic

tick :: GameState -> GameState
tick s = tickLiving s <> tickDead s

tickLiving :: GameState -> GameState
tickLiving (GameState w m) = GameState w $ M.fromList $ M.foldrWithKey' f [] m
  where f pos cell xs = let n = neighbours w pos m
                        in if rule2 cell n || rule4 cell n
                              then (pos, Live):xs
                              else xs

tickDead :: GameState -> GameState
tickDead (GameState w m) = GameState w $ M.fromList $ M.foldrWithKey' f [] m
  where f pos cell xs = let n = neighbours w pos m
                        in if rule1 cell n || rule3 cell n
                              then (pos, Dead):xs
                              else xs

-- 1. Any live cell with fewer than two live neighbours dies, as if by underpopulation.
rule1 :: Cell -> [(Pos, Cell)] -> Bool
rule1 Live = (< 2) . length . filter (\(_, c) -> isAlive c)
rule1 Dead = const False

-- 2. Any live cell with two or three live neighbours lives on to the next generation.
rule2 :: Cell -> [(Pos, Cell)] -> Bool
rule2 Live = (> 1) . length . filter (\(_, c) -> isAlive c)
rule2 Dead = const False

-- 3. Any live cell with more than three live neighbours dies, as if by overpopulation.
rule3 :: Cell -> [(Pos, Cell)] -> Bool
rule3 Live = (> 3) . length . filter (\(_, c) -> isAlive c)
rule3 Dead = const False

-- 4. Any dead cell with exactly three live neighbours becomes a live cell, as if by reproduction.
rule4 :: Cell -> [(Pos, Cell)] -> Bool
rule4 Dead = (==) 3 . length . filter (\(_, c) -> isAlive c)
rule4 Live = const False

neighbours :: Int -> Pos -> M.Map Pos Cell -> [(Pos, Cell)]
neighbours w p m = M.toList $ M.intersection m d
  where
    d = M.fromList [ (p-1-w, ()), (p-w, ()), (p+1-w, ())
                   , (p-1, ()),              (p+1, ())
                   , (p-1+w, ()), (p+w, ()), (p+1+w, ())]

isAlive :: Cell -> Bool
isAlive Live = True
isAlive Dead = False


