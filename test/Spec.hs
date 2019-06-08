import Test.Hspec
import Test.Hspec.Contrib.HUnit (fromHUnitTest)
import Test.HUnit

import Lib

main :: IO ()
main = hspec $ do
  describe "tick" $ do
    fromHUnitTest tickTests
  describe "neighbours" $ do
    fromHUnitTest neighboursTests


tickTests :: Test
tickTests = TestList [ TestLabel "test1" tickTest1
                     , TestLabel "test2" tickTest2
                     ]

neighboursTests :: Test
neighboursTests = TestList [ TestLabel "test1" neighboursTest1
                           , TestLabel "test2" neighboursTest2
                           ]

tickTest1 :: Test
tickTest1 = TestCase $ do
  1 @?= 1

tickTest2 :: Test
tickTest2 = TestCase $ do
  "hello" @?= "hello"

neighboursTest1 :: Test
neighboursTest1 = TestCase $ do
  1 @?= 1

neighboursTest2 :: Test
neighboursTest2 = TestCase $ do
  2 @?= 2
