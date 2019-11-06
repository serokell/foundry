module Source.Plugin
  ( module Source.Plugin.Precedence,
    module Sdam.Syn,
    module Sdam.Validator,
    ShapeName (..),
    Plugin (..),
  )
where

import Data.HashMap.Strict (HashMap)
import Data.Primitive.Array (Array)
import Data.Text (Text)
import Sdam.Syn (SynShape)
import Sdam.Validator (Schema (..), mkTyUnion)
import Source.Plugin.Precedence

data ShapeName
  = ShapeName
      { shapeName :: Text,
        shapeFieldNames :: Array Text
      }

data Plugin
  = Plugin
      { pluginSchema :: Schema,
        pluginPrecInfo :: HashMap SynShape (Array PrecPredicate),
        pluginShapeNames :: HashMap SynShape ShapeName
      }
