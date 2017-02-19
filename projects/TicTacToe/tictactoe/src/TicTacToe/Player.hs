module TicTacToe.Player where

data Player = X | O 
    deriving (Eq, Ord, Show)

swapPlayer :: Player -> Player
swapPlayer X = O
swapPlayer O = X

