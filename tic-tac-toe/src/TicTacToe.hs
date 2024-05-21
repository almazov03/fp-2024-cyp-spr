{-# LANGUAGE OverloadedStrings #-}

module TicTacToe (playGame) where

import Data.List (intersperse, transpose)
import Data.Char (isDigit)
import Control.Monad (when)
import System.IO (hFlush, stdout)

data Player = X | O | PlayerN Int deriving (Eq, Show)
type Board = [[Maybe Player]]

playGame :: IO ()
playGame = do
    size <- getBoardSize
    numPlayers <- getNumPlayers
    gameLoop size (replicate size (replicate size Nothing)) (cyclePlayers numPlayers)

getBoardSize :: IO Int
getBoardSize = do
    putStr "Enter board size: "
    hFlush stdout
    input <- getLine
    let size = read input
    if size < 3
        then do
            putStrLn "Minimum board size is 3"
            getBoardSize
        else return size

getNumPlayers :: IO Int
getNumPlayers = do
    putStr "Enter number of players: "
    hFlush stdout
    input <- getLine
    let num = read input
    if num < 2
        then do
            putStrLn "Minimum number of players is 2"
            getNumPlayers
        else return num

cyclePlayers :: Int -> [Player]
cyclePlayers n = map PlayerN [1..n]

gameLoop :: Int -> Board -> [Player] -> IO ()
gameLoop size board (player:players) = do
    printBoard board
    if checkWin board
        then putStrLn $ "Player " ++ show player ++ " wins!"
        else if isFull board
            then putStrLn "It's a draw!"
            else do
                putStrLn $ "Player " ++ show player ++ "'s turn."
                move <- getMove size
                case move of
                    Just (r, c) -> do
                        let newBoard = updateBoard board r c player
                        if newBoard == board
                            then do
                                putStrLn "Invalid move, try again."
                                gameLoop size board (player:players)
                            else gameLoop size newBoard players
                    Nothing -> do
                        putStrLn "Invalid input, try again."
                        gameLoop size board (player:players)

getMove :: Int -> IO (Maybe (Int, Int))
getMove size = do
    putStr "Enter your move (row and column): "
    hFlush stdout
    input <- getLine
    return $ parseMove size input

parseMove :: Int -> String -> Maybe (Int, Int)
parseMove size input = case words input of
    [r, c] | all isDigit r && all isDigit c ->
        let row = read r
            col = read c
        in if row >= 0 && row < size && col >= 0 && col < size
            then Just (row, col)
            else Nothing
    _ -> Nothing

updateBoard :: Board -> Int -> Int -> Player -> Board
updateBoard board r c player =
    if board !! r !! c == Nothing
        then take r board ++
             [take c (board !! r) ++ [Just player] ++ drop (c + 1) (board !! r)] ++
             drop (r + 1) board
        else board

isFull :: Board -> Bool
isFull = all (all (/= Nothing))

checkWin :: Board -> Bool
checkWin board = any isLine (rows ++ cols ++ diags)
  where
    rows = board
    cols = transpose board
    diags = [diag board, diag (map reverse board)]
    isLine line = all (== Just (PlayerN 1)) line || all (== Just (PlayerN 2)) line

diag :: [[a]] -> [a]
diag b = [b !! n !! n | n <- [0..length b - 1]]

printBoard :: Board -> IO ()
printBoard board = do
    let display = map (map displayCell) board
    mapM_ (putStrLn . concat . intersperse " | ") display
    putStrLn ""

displayCell :: Maybe Player -> String
displayCell Nothing = " "
displayCell (Just X) = "X"
displayCell (Just O) = "O"
displayCell (Just (PlayerN n)) = show n

