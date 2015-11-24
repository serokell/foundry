{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Node where

import Data.Map (Map)
import Control.Lens

data Node rep rel label
  = Rep
    { _nodeLabel :: label
    , _nodeRep :: rep
    }
  | Node
    { _nodeLabel :: label
    , _nodeSlaves :: Map rel (Node rep rel label)
    }
  deriving (Eq)

makeLenses ''Node

newtype Path rel = Path [rel]
  deriving (Eq, Monoid)

makePrisms ''Path

atPath :: Ord rel => Path rel -> Traversal' (Node rep rel label) (Node rep rel label)
atPath path = case path ^. _Path of
  [] -> id
  (r:rs) -> nodeSlaves . at r . _Just . atPath (_Path # rs)
