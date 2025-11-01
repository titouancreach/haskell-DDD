module Domain.Ascii where

import qualified Data.Text as T
import qualified Domain.Url as Url

newtype Ascii = Ascii
  { content :: T.Text
  }
  deriving (Show, Eq)

class (Monad m) => ImageUrlToAscii m where
  imageUrlToAscii :: Url.ImageUrl -> m (Either T.Text Ascii)
