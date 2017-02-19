{-# LANGUAGE ScopedTypeVariables #-}

module TicTacToe.Board where

import Data.Maybe
import TicTacToe.Player
import Control.Monad.State

data Coordinate = 
    Zero
  | One
  | Two
  deriving (Show, Eq, Ord, Enum, Bounded)

fromCoor :: Coordinate -> Int 
fromCoor Zero = 0
fromCoor One  = 1
fromCoor Two  = 2

toCoor :: Int -> Maybe Coordinate
toCoor 0 = Just Zero
toCoor 1 = Just One
toCoor 2 = Just Two
toCoor _ = Nothing

type Position = (Coordinate, Coordinate)

fromPos :: Position -> (Int, Int)
fromPos (x, y) = (fromCoor x, fromCoor y)

toPos :: (Int, Int) -> Maybe Position
toPos (x, y) = do 
    xC <- toCoor x
    yC <- toCoor y
    return (xC, yC)


type Step = (Player, Position)

type Board = ([Step], [[Maybe Player]])


putChess :: Player -> Position -> [[Maybe Player]] -> [[Maybe Player]]
putChess p pos board = 
    case sequenceA $ map sequenceA board of Just _ -> error "Move on an finished Board"
                                            Nothing -> case board !! x !! y of Nothing -> ls ++ [newx] ++ rs
                                                                               _ -> error "It already occupied"
        where (x,y)  = fromPos pos
              ls     = take x board
              (xs:rs) = drop x board
              newx   = take y xs ++ [Just p] ++ tail (drop y xs)



move :: Player -> Position -> State Board (Maybe Step)
move p pos = do
    (steps, board) <- get
    let newBoard = putChess p pos board
    let newSteps = (p, pos):steps
    put (newSteps, newBoard)
    return (Just (p, pos))


