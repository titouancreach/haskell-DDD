module Domain.Ascii where

import Data.Text (Text)

import qualified Domain.Url as Url

newtype Ascii = Ascii
  { content :: Text
  }
  deriving (Show, Eq)

class (Monad m) => ImageUrlToAscii m where
  imageUrlToAscii :: Url.ImageUrl -> m (Either Text Ascii)
