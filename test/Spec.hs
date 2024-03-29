import Test.Hspec
import Test.Hspec.Contrib.HUnit (fromHUnitTest)
import Test.HUnit

import Lib

main :: IO ()
main = hspec $ do
  describe "tick" $ tickTest
  describe "neighbours" $ do
    fromHUnitTest neighboursTests
  describe "overflow" $ do
    fromHUnitTest overflowTests

tickTest :: Spec
tickTest = it "tick" $ do
  let g = grid 3 [ 1, 0, 1
                 , 0, 1, 0
                 , 1, 0, 1
                 ]
  tick g @?= grid 3 [ 0, 1, 0
                    , 1, 0, 1
                    , 0, 1, 0
                    ]

neighboursTests :: Test
neighboursTests = TestList [ TestLabel "test1" neighboursTest1
                           , TestLabel "test2" neighboursTest2
                           ]

overflowTests :: Test
overflowTests = TestList [ TestLabel "test1" overflowTest1
                         , TestLabel "test2" overflowTest2
                         ]

neighboursTest1 :: Test
neighboursTest1 = TestCase $ do
  let g = grid 3 [ 1, 0, 1
                 , 0, 1, 0
                 , 1, 0, 1
                 ]
  neighbours g 7 @?= [Dead, Live, Dead]

neighboursTest2 :: Test
neighboursTest2 = TestCase $ do
  let g = grid 3 [ 1, 0, 1
                 , 0, 1, 0
                 , 1, 0, 1
                 ]
  neighbours g 5 @?= [ Live, Dead, Live
                     , Dead,       Dead
                     , Live, Dead, Live
                     ]

overflowTest1 :: Test
overflowTest1 = TestCase $ do
  hOverflow 3 4 3 @?= True

overflowTest2 :: Test
overflowTest2 = TestCase $ do
  hOverflow 3 9 10 @?= True
