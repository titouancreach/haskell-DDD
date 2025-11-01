module Domain.Url where

import Data.Text (Text)

newtype Url = Url {unUrl :: Text}
  deriving (Show, Eq, Ord)

newtype ImageUrl = ImageUrl {unImageUrl :: Text}
  deriving (Show, Eq, Ord)
