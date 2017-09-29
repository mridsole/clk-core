{-# LANGUAGE TemplateHaskell #-}

module World.Entity where
--import qualified Entity.Generated

import Data.DeriveTH

type EntityID = Int

-- TODO: Move this elsewhere.
type PlayerID = Int

data BlinkerState = On | Off
  deriving (Show)

data EntityType =
  Observer |
  Blinker BlinkerState
  deriving (Show)

data Entity = Entity {
  entityID :: EntityID,
  ownerID :: PlayerID,
  entityType :: EntityType
  } deriving (Show)

-- Render an entity.
-- TODO: submodule for ASCII rendering?
render :: Entity -> Char
render (Entity _ _ Observer) = 'O'
render (Entity _ _ (Blinker On)) = '/'
render (Entity _ _ (Blinker Off)) = '\\'

-- um

-- module Entity.Generated where
-- 
-- derive makeIs ''Entity
