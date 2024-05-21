import Test.HUnit
import Test.QuickCheck
import TicTacToe

main :: IO ()
main = do
    _ <- runTestTT tests
    quickCheck prop_parseMove
    quickCheck prop_isFull

tests :: Test
tests = TestList [
    "parseMove" ~: parseMove 3 "1 2" ~?= Just (1, 2),
    "parseMove out of bounds" ~: parseMove 3 "3 3" ~?= Nothing,
    "isFull" ~: isFull (replicate 3 (replicate 3 (Just (PlayerN 1)))) ~?= True,
    "isFull not full" ~: isFull (replicate 3 (replicate 3 Nothing)) ~?= False,
    "checkWin row" ~: checkWin [[Just (PlayerN 1), Just (PlayerN 1), Just (PlayerN 1)], [Nothing, Nothing, Nothing], [Nothing, Nothing, Nothing]] ~?= True,
    "checkWin col" ~: checkWin [[Just (PlayerN 1), Nothing, Nothing], [Just (PlayerN 1), Nothing, Nothing], [Just (PlayerN 1), Nothing, Nothing]] ~?= True,
    "checkWin diag" ~: checkWin [[Just (PlayerN 1), Nothing, Nothing], [Nothing, Just (PlayerN 1), Nothing], [Nothing, Nothing, Just (PlayerN 1)]] ~?= True,
    "updateBoard valid" ~: updateBoard (replicate 3 (replicate 3 Nothing)) 1 1 (PlayerN 1) ~?= [[Nothing, Nothing, Nothing], [Nothing, Just (PlayerN 1), Nothing], [Nothing, Nothing, Nothing]],
    "updateBoard invalid" ~: updateBoard [[Nothing, Nothing, Nothing], [Nothing, Just (PlayerN 1), Nothing], [Nothing, Nothing, Nothing]] 1 1 (PlayerN 2) ~?= [[Nothing, Nothing, Nothing], [Nothing, Just (PlayerN 1), Nothing], [Nothing, Nothing, Nothing]]
    ]

prop_parseMove :: Int -> Int -> Property
prop_parseMove size r

