module Lib
    ( run
    , GameState(..)
    , Cell(..)
    , gamestate
    , tick
    , neighbours
    , overflow
    ) where

import qualified Data.Map.Strict as M
import Data.List (intercalate)
import Data.List.Split (chunksOf)
import Control.Monad (join, void)
import Control.Concurrent (threadDelay)

-- Types

data GameState = GameState { gWidth :: Int
                           , grid :: M.Map Pos Cell
                           }
  deriving Eq

instance Show GameState where
  show (GameState w g) =
    intercalate "\n" . chunksOf w . join . fmap show . M.elems $ g

gamestate :: Int -> [Int] -> GameState
gamestate w = GameState w . M.fromList . zip [1..] . fmap f
  where f 0 = Dead
        f _ = Live

data Cell = Live | Dead
  deriving Eq

instance Semigroup Cell where
  Live <> Live = Live
  _ <> _       = Dead

instance Show Cell where
  show Live = "x"
  show Dead = "_"

type Pos = Int

-- Run

run :: GameState -> IO ()
run g = void $ loop g

loop :: GameState -> IO GameState
loop g = do
  display g
  loop (tick g)

display :: GameState -> IO ()
display g = do
  putStrLn (ansiClearScreen ++ ansiGoto 1 1 ++ show g)
  threadDelay 1000000

ansiClearScreen = "\ESC[2J"

ansiGoto :: Int -> Int -> String
ansiGoto x y = "\ESC[" ++ show y ++ ";" ++ show x ++ "H"

-- Logic

-- 1. Any live cell with fewer than two live neighbours dies, as if by underpopulation.
-- 2. Any live cell with two or three live neighbours lives on to the next generation.
-- 3. Any live cell with more than three live neighbours dies, as if by overpopulation.
-- 4. Any dead cell with exactly three live neighbours becomes a live cell, as if by reproduction.
tick :: GameState -> GameState
tick s = apply [ mkRule Live (activation (< 2)) Dead
               , mkRule Live (activation (\n -> n == 2 || n == 3)) Live
               , mkRule Live (activation (> 3)) Dead
               , mkRule Dead (activation ((==) 3)) Live
               ] s

newtype Rule = Rule (Cell -> [Cell] -> Maybe Cell)

activation :: (Int -> Bool) -> [Cell] -> Bool
activation pred = pred . length . filter isAlive

apply :: [Rule] -> GameState -> GameState
apply rs g@(GameState w m) = GameState w . M.fromList $ M.foldrWithKey' f [] m
  where f pos cell xs = let cs = neighbours g pos
                        in case mconcat (map (\(Rule f) -> f cell cs) rs) of -- ?
                             Just c  -> (pos, c):xs
                             Nothing -> (pos, cell):xs

mkRule :: Cell -> ([Cell] -> Bool) -> Cell -> Rule
mkRule start pred end = Rule $ \c' cs ->
  if start == c' && (pred cs)
     then Just end
     else Nothing

neighbours :: GameState -> Pos -> [Cell]
neighbours (GameState w m) p = M.elems $ M.intersection m m'
  where
    m' = M.fromList . filter (not . overflow w p) $ potentialNeighbours w p

potentialNeighbours w p = [ (p-1-w, ()), (p-w, ()), (p+1-w, ())
                          , (p-1, ()),              (p+1, ())
                          , (p-1+w, ()), (p+w, ()), (p+1+w, ())]

overflow :: Int -> Pos -> (Pos, a) -> Bool
overflow w p (p', _) = let isRightBorder = (p `mod` w) == 0
                           isLeftBorder = ((p - 1) `mod` w) == 0
                       in if isRightBorder
                             then any (\p'' -> p'' == p') [p + 1, p + 1 - w, p + 1 + w]
                             else if isLeftBorder
                                     then any (\p'' -> p'' == p') [p - 1, p - 1 - w, p - 1 + w]
                                     else False

isAlive :: Cell -> Bool
isAlive Live = True
isAlive Dead = False
