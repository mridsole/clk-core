module World
  ( WIx
  , EntityID
  , Entity
  , Slot (..)
  , Chunk
  , ChunkBounds (..)
  , chunkSize
  , chunkBounds
  , chunkConst
  , nextStateMin
  , chunkRender
  ) where

import Control.Exception.Base
import Data.Array
import Entity
import Data.List (intersperse)

-- An index to a point in the world.
type WIx = (Int, Int)

-- Possible contents of a world point.
data Slot =
  Space [Entity] |
  Wall
  deriving (Show)

-- A square subset of the world.
type Chunk = Array WIx Slot

-- Describes the top left corner and the size of the chunk.
data ChunkBounds = ChunkBounds WIx Int
  deriving (Show, Eq)

-- Get the size of a chunk.
chunkSize :: Chunk -> Int
chunkSize chunk = let ((ax,ay),(bx,by)) = bounds chunk in
  if ((ax - bx) == (ay - by))
    then abs (ax - bx) + 1
    else throw $ ErrorCall "Chunks must be square"

chunkBounds :: Chunk -> ChunkBounds
chunkBounds chunk = let ((ax,ay), (bx,by)) = bounds chunk in
  ChunkBounds (ax,ay) $ chunkSize chunk

-- Chunk filled will a constant slot.
chunkConst :: WIx -> Int -> Slot -> Chunk
chunkConst (x,y) size slot = array ((x,y), (x+size-1,y+size-1))
  [((i,j), slot) | i <- [x..(x+size-1)], j <- [y..(y+size-1)]]

{-- State transition on an (n,n) chunk to obtain a (n-2, n-2) chunk.
    Eventually, this will need a way of incorporating inputs / commands /
    actions or whatever ...
    --}
-- nextState :: Chunk -> Chunk
-- nextState = 10

-- The minimal case (for a 3x3 chunk)
nextStateMin :: Chunk -> Slot
nextStateMin = undefined

slotRender :: Slot -> Char
slotRender Wall = 'W'
slotRender (Space _) = ' '

intersperseK :: a -> [a] -> Int -> [a]
intersperseK x xs k = let
  (as,bs) = splitAt k xs
  na = length bs in
  case compare na 0 of
    EQ -> as
    otherwise -> (as ++ [x]) ++ (intersperseK x bs k)

-- Render a chunk to ASCII.
chunkRender :: Chunk -> String
chunkRender chunk = let
  n = chunkSize chunk
  slots = elems $ fmap slotRender chunk in
  (intersperseK '\n' (intersperse ' ' slots) (2*n))
