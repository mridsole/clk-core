module World
  ( WIx
  , EntityID
  , Entity
  , Slot (..)
  , Chunk
  , ChunkBounds (..)
  , chunkSize
  -- , chunkBounds
  , chunkConst
  , nextStateMinor
  , chunkRender
  ) where

import Control.Exception.Base
import Data.Array
import Entity
import qualified Entity (render)
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

-- etc
type ChunkBounds = (WIx, WIx)

type CoordMap = Array WIx WIx

-- Get the size of a chunk.
chunkSize :: Chunk -> Int
chunkSize chunk = let ((ax,ay),(bx,by)) = bounds chunk in
  if ((ax - bx) == (ay - by))
    then abs (ax - bx) + 1
    else throw $ ErrorCall "Chunks must be square"

-- chunkBounds :: Chunk -> ChunkBounds
-- chunkBounds chunk = let ((ax,ay), (bx,by)) = bounds chunk in
--   ChunkBounds (ax,ay) $ chunkSize chunk

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
-- nextStateMin :: Chunk -> Slot
-- nextStateMin = undefined

slotRender :: Slot -> Char
slotRender Wall = 'W'
slotRender (Space (ent:ents)) = Entity.render ent

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

-- Is there an observer at this chunk/index?
-- observerAt chunk wix = or (

-- Run a minor tick state update.
nextStateMinor :: Chunk -> Chunk
nextStateMinor chunk = fmap nextStateMinor3 $ neighbourhoods chunk

nextStateMinor3 :: Chunk -> Slot
nextStateMinor3 chunk = assert (chunkSize chunk == 3) $ let
  ((ax,ay),_) = bounds chunk
  midslot = chunk ! (ax+1,ay+1) in
  case midslot of
    Space ((Entity a b (Blinker Off)):xs) -> Space (Entity a b (Blinker On):xs)
    Space ((Entity a b (Blinker On)):xs) -> Space (Entity a b (Blinker Off):xs)
    otherwise -> midslot

neighbourhood :: WIx -> Chunk -> Chunk
neighbourhood at chunk = assert (at `inInterior` bounds chunk) $
  let (x,y) = at in
  ixmap ((x-1,y-1),(x+1,y+1)) id chunk
  where
  (x,y) `inInterior` ((ax,ay),(bx,by)) =
    (x > ax && x < bx) && (y > ay && y < by)

interiorBounds :: Chunk -> ChunkBounds
interiorBounds chunk = let ((ax,ay),(bx,by)) = bounds chunk in
  ((ax+1,ay+1),(bx-1,by-1))

interior :: Chunk -> Chunk
interior chunk = assert (chunkSize chunk >= 3) $
  ixmap (interiorBounds chunk) id chunk

coordMap :: ChunkBounds -> CoordMap
coordMap ((ax,ay),(bx,by)) = assert (ax <= bx && ay <= by) $
  array ((ax,ay),(bx,by)) [((i,j),(i,j)) | i <- [ax..bx], j <- [ay..by]]

neighbourhoods chunk = fmap (flip neighbourhood chunk) $
  coordMap $ interiorBounds chunk
