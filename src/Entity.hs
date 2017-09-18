module Entity
  ( Entity,
    EntityID
  ) where

type EntityID = Int

-- TODO: World entity that can exist in space
data Entity = Observer EntityID
  deriving (Show)
