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


emptyBoard :: Board
emptyBoard = ([], replicate 3 $ replicate 3 Nothing)


chessBoard :: State Board a -> [[Maybe Player]]
chessBoard bs = let (_, (_, board)) =  runState (bs >>= const get) emptyBoard
                in board

playerAt :: Position -> State Board a -> Maybe Player
playerAt pos bs = board !! x !! y
    where (x,y) = fromPos pos
          board = chessBoard bs


compress  :: Eq a => [a] -> Maybe a
compress [] = Nothing
compress (x:xs) = if all (==x) xs then Just x else Nothing

findFirst :: (a -> Bool) -> [a] -> Maybe a
findFirst _ [] = Nothing
findFirst f (x:xs) = if f x then Just x else findFirst f xs

whoWon :: State Board a -> Maybe Player
whoWon bs = join $ findFirst (/= Nothing) [fRow, sRow, tRow, fCol, sCol, tCol, xx, yy]
    where board = chessBoard bs
          fRow = sequenceA (board !! 0) >>= compress
          sRow = sequenceA (board !! 1) >>= compress
          tRow = sequenceA (board !! 2) >>= compress
          fCol = sequenceA (map (!! 0) board) >>= compress
          sCol = sequenceA (map (!! 1) board) >>= compress
          tCol = sequenceA (map (!! 2) board) >>= compress
          xx   = sequenceA (zipWith (!!) board [0,1,2]) >>= compress
          yy   = sequenceA (zipWith (!!) board [2,1,0]) >>= compress



isDraw :: State Board a -> Bool
isDraw bs = isJust $ sequenceA $ map sequenceA board
    where board = chessBoard bs
