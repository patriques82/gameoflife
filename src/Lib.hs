module Lib
    ( run
    , Seed(..)
    ) where

import qualified Data.Map.Strict as M
import Data.List (intercalate)
import Data.List.Split (chunksOf)
import System.Random (randomRs, newStdGen, RandomGen)
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

instance Show GameState where
  show (GameState w g) =
    intercalate "\n" . chunksOf w . join . fmap show . M.elems $ g

data Cell = Live | Dead
  deriving Eq

instance Semigroup Cell where
  Live <> _    = Live
  _ <> Live    = Live
  Dead <> Dead = Dead

instance Show Cell where
  show Live = "X"
  show Dead = " "

type Pos = Int

-- Run

run :: Seed -> IO ()
run s = void $ initGame s >>= loop

initGame :: Seed -> IO GameState
initGame s = newStdGen >>= \g ->
  return $ GameState (width s) (alive g s <> dead s)

loop :: GameState -> IO GameState
loop g = do
  display g
  loop (tick g)

display :: GameState -> IO ()
display g = do
  putStrLn (ansiClearScreen ++ ansiGoto 1 1 ++ show g)
  threadDelay 1000000

ansiClearScreen :: String
ansiClearScreen = "\ESC[2J"

ansiGoto :: Int -> Int -> String
ansiGoto x y = "\ESC[" ++ show y ++ ";" ++ show x ++ "H"

total s = width s * height s

--

alive :: RandomGen a => a -> Seed -> M.Map Pos Cell
alive g s = M.fromList $ take (liveCells s) ls
  where ls = randomRs (0, total s) g `zip` repeat Live

dead :: Seed -> M.Map Pos Cell
dead s = M.fromList $ take (total s - liveCells s) ds
  where ds = [(p, c) | p <- [1..total s], c <- repeat Dead]

-- Logic

-- 1. Any live cell with fewer than two live neighbours dies, as if by underpopulation.
-- 2. Any live cell with two or three live neighbours lives on to the next generation.
-- 3. Any live cell with more than three live neighbours dies, as if by overpopulation.
-- 4. Any dead cell with exactly three live neighbours becomes a live cell, as if by reproduction.
tick :: GameState -> GameState
tick s = apply [ mkRule Live (< 2) Dead
               , mkRule Live (\n -> n == 2 || n == 3) Live
               , mkRule Live (> 3) Dead
               , mkRule Dead ((==) 3) Live
               ] s

newtype Rule = Rule (Cell -> [Cell] -> Maybe Cell)

apply :: [Rule] -> GameState -> GameState
apply rs (GameState w m) = GameState w . M.fromList $ M.foldrWithKey' f [] m
  where f pos cell xs = let cs = neighbours w pos m
                        in case mconcat (map (\(Rule f) -> f cell cs) rs) of -- ?
                             Just c  -> (pos, c):xs
                             Nothing -> xs



mkRule :: Cell -> ([Cell] -> Bool) -> Cell -> Rule
mkRule start pred end = Rule $ \c' cs ->
  if start == c' && (pred . length . filter isAlive $ cs)
     then Just end
     else Nothing

neighbours :: Int -> Pos -> M.Map Pos Cell -> [Cell]
neighbours w p m = M.elems $ M.intersection m d
  where
    d = M.fromList [ (p-1-w, ()), (p-w, ()), (p+1-w, ())
                   , (p-1, ()),              (p+1, ())
                   , (p-1+w, ()), (p+w, ()), (p+1+w, ())]

isAlive :: Cell -> Bool
isAlive Live = True
isAlive Dead = False
