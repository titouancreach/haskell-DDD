module Domain.Ascii where

import Data.Text (Text)

newtype Ascii = Ascii
  { content :: Text
  }
  deriving (Show, Eq)
