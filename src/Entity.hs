{-# LANGUAGE TemplateHaskell #-}

module Entity where
--import qualified Entity.Generated

import Data.DeriveTH

type EntityID = Int

data BlinkerState = On | Off
  deriving (Show)

data EntityType =
  Observer |
  Blinker BlinkerState
  deriving (Show)

data Entity = Entity {
  entityID :: EntityID,
  entityType :: EntityType
  } deriving (Show)

-- Render an entity.
-- TODO: submodule for ASCII rendering?
render :: Entity -> Char
render (Entity _ Observer) = 'O'
render (Entity _ (Blinker On)) = '/'
render (Entity _ (Blinker Off)) = '\\'

-- um

-- module Entity.Generated where
-- 
-- derive makeIs ''Entity
