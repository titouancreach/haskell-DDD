module Domain.Url where

import qualified Data.Text as T

newtype Url = Url {unUrl :: T.Text}
  deriving (Show, Eq, Ord)

newtype ImageUrl = ImageUrl {unImageUrl :: T.Text}
  deriving (Show, Eq, Ord)
