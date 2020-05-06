module Main where

import ClassyPrelude

import qualified Data.Map as Map
import Data.Map (Map)

import Data.List ((!!))

type Players = Int
type Marbles = Int
type HighScore = Int

type Player = Int

type Board = [Int]

type PrevMarbleNum = Int
type NewMarbleNum = Int

findIndex :: PrevMarbleNum -> Board -> Int
findIndex prevMarbleNum board = go board 0
  where
    go :: Board -> Int -> Int
    go [] _ = error "not found"
    go (h:t) i
      | h == prevMarbleNum = i
      | otherwise = go t (i + 1)

insertAt :: Int -> a -> [a] -> [a]
insertAt 0 a as = a : as
insertAt n a (h:t) = h : insertAt (n - 1) a t

insertMarble :: PrevMarbleNum -> NewMarbleNum -> Board -> Board
insertMarble prevMarbleNum newMarbleNum board =
  let indexOfPrevMarble = findIndex prevMarbleNum board
      indexOfNext = (indexOfPrevMarble + 2) `mod` length board
  in insertAt indexOfNext newMarbleNum board

type Score = Int
type Scores = Map Player Score

type NewCurrentMarble = Int

type MarbleRemovedNum = Int

removeIndex :: Int -> Board -> (MarbleRemovedNum, Board)
removeIndex 0 (h:t) = (h, t)
removeIndex n (h:t) = let (ff, tt) = removeIndex (n - 1) t in (ff, h:tt)

updateScores :: Player -> Score -> Scores -> Scores
updateScores p s scores = insertWith (+) p s scores

findAtIndex :: Int -> Board -> Int
findAtIndex i b =
  -- trace ("findAtIndex, i: " <> show i <> ", b: " <> show b)
  (b !! i)

do23
  :: Players
  -> Scores
  -> Board
  -> PrevMarbleNum
  -> NewMarbleNum
  -> (Scores, Board, NewCurrentMarble)
do23 players scores board prevMarbleNum newMarbleNum =
  let prevMarbleNumIndex = findIndex prevMarbleNum board
      marbleToRemoveIndex = (prevMarbleNumIndex - 7) `mod` length board
      (removedMarbleNum, newBoard) = removeIndex marbleToRemoveIndex board
      scoreToAdd = newMarbleNum + removedMarbleNum
      playerToAddScoreTo = newMarbleNum `mod` players
      newScores = updateScores playerToAddScoreTo scoreToAdd scores
      newCurrentMarble = findAtIndex marbleToRemoveIndex newBoard
  in
  -- trace (
  --   "do23, players: " <> show players <>
  --   " scores: " <> show scores <>
  --   " board: " <> show board <>
  --   " prevMarbleNum: " <> show prevMarbleNum <>
  --   " newMarbleNum: " <> show newMarbleNum <>
  --   " prevMarbleNumIndex: " <> show prevMarbleNumIndex <>
  --   " marbleToRemoveIndex: " <> show marbleToRemoveIndex
  --   )
  (newScores, newBoard, newCurrentMarble)

createBoard :: Players -> Marbles -> (Scores, Board)
createBoard players lastMarble = go Map.empty [0] 0 1
  where
  go :: Scores -> Board -> PrevMarbleNum -> NewMarbleNum -> (Scores, Board)
  go scores board 0 1 = go scores [0,1] 1 2
  go scores board prevMarbleNum newMarbleNum
    | newMarbleNum > lastMarble = (scores, board)
    | newMarbleNum `mod` 23 == 0 =
        let (newScores, newBoard, newCurrentMarble) =
              do23 players scores board prevMarbleNum newMarbleNum
        in go newScores newBoard newCurrentMarble (newMarbleNum + 1)
    | otherwise =
        go
          scores
          (insertMarble prevMarbleNum newMarbleNum board)
          newMarbleNum
          (newMarbleNum + 1)

-- simGame :: Players -> Marbles -> HighScore
-- simGame players lastMarble = go [0] 1
--   where
--   go :: [Int] -> Player -> HighScore
--   go board ps | ps == players = board
--   go board 0 = go [0]

main :: IO ()
main = do
  let players = 9
      lastMarble = 25
      -- highScore = simGame [0]
      board = createBoard players lastMarble
  -- print highScore
  pure ()
