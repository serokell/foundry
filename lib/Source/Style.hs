module Source.Style where

import Data.Text (Text)

data Color = RGB Double Double Double

data FontWeight = FontWeightBold | FontWeightNormal

data Font = Font
    { fontFamily :: Text
    , fontSize :: Double
    , fontColor :: Color
    , fontWeight :: FontWeight
    }
