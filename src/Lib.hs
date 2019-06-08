module Lib
    ( run
    , runTest
    , Seed(..)
    , GameState(..)
    , tick
    , neighbours
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

test :: GameState -> String -> Bool
test g s = (show g) == s

data Cell = Live | Dead
  deriving Eq

instance Semigroup Cell where
  Live <> Live = Live
  _ <> _       = Dead

instance Show Cell where
  show Live = "X"
  show Dead = "O"

type Pos = Int

-- Run

runTest :: IO ()
runTest = do
  putStrLn (show g)
  putStrLn (show (tick g)) --(show (test g s))
  where g = GameState 3 (M.fromList [ (1, Live), (2, Live), (3, Dead)
                                    , (4, Dead), (5, Dead), (6, Live)
                                    , (7, Live), (8, Live), (9, Dead)
                                    ])
        s = "OXO\nOOX\nOXO"

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

ansiClearScreen = "\ESC[2J"

ansiGoto :: Int -> Int -> String
ansiGoto x y = "\ESC[" ++ show y ++ ";" ++ show x ++ "H"

--

alive :: RandomGen a => a -> Seed -> M.Map Pos Cell
alive g s = M.fromList $ take (liveCells s) ls
  where ls = randomRs (1, total s) g `zip` repeat Live

dead :: Seed -> M.Map Pos Cell
dead s = M.fromList $ take (total s - liveCells s) ds
  where ds = [1..total s] `zip` repeat Dead

total s = width s * height s

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
apply rs (GameState w m) = GameState w . M.fromList $ M.foldrWithKey' f [] m
  where f pos cell xs = let cs = neighbours w pos m
                        in case mconcat (map (\(Rule f) -> f cell cs) rs) of -- ?
                             Just c  -> (pos, c):xs
                             Nothing -> (pos, cell):xs

mkRule :: Cell -> ([Cell] -> Bool) -> Cell -> Rule
mkRule start pred end = Rule $ \c' cs ->
  if start == c' && (pred cs)
     then Just end
     else Nothing

neighbours :: Int -> Pos -> M.Map Pos Cell -> [Cell]
neighbours w p m = M.elems . M.intersection m . M.fromList . filter (overflow w p) $ d
  where
    d = [ (p-1-w, ()), (p-w, ()), (p+1-w, ())
        , (p-1, ()),              (p+1, ())
        , (p-1+w, ()), (p+w, ()), (p+1+w, ())]

overflow :: Int -> Pos -> (Pos, a) -> Bool
overflow w p (p', _) = let isRightBorder = p `mod` w == 0
                           isLeftBorder = p - 1 `mod` w == 0
                       in if isRightBorder
                             then all (\p'' -> p'' /= p') [p + 1, p + 1 - w, p + 1 + w]
                             else if isLeftBorder
                                     then all (\p'' -> p'' /= p') [p - 1, p - 1 - w, p - 1 + w]
                                     else True

isAlive :: Cell -> Bool
isAlive Live = True
isAlive Dead = False
