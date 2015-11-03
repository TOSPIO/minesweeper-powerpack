{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiWayIf #-}

module Game where

import qualified Data.Vector as V

data Brick = Brick {
  isBomb :: Bool
  } deriving Show

type Row = V.Vector Brick

data Board where
  Board :: V.Vector Row -> Board
  deriving Show


row :: Board -> Int -> Maybe Row
row (Board d) index
  | index < 0 = Nothing
  | index >= V.length d = Nothing
  | otherwise = Just $ d V.! index

brick :: Board -> Int -> Int -> Maybe Brick
brick board rowIdx colIdx = do
  r <- row board rowIdx
  if
    | colIdx < 0 -> Nothing
    | colIdx >= V.length r -> Nothing
    | otherwise -> Just $ r V.! colIdx
