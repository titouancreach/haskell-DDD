module Domain.Ascii (Ascii (..), Error (..)) where

import Data.Text (Text)

data Error = ExternalApiError Text

newtype Ascii = Ascii
  { content :: Text
  }
  deriving (Show, Eq)
