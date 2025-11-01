{-# LANGUAGE MultiParamTypeClasses #-}

module Application.Ascii.Capability where

import Data.Text (Text)

import qualified Domain.Ascii as Ascii
import qualified Domain.Url as Url

-- | Capability type class for converting images to ASCII
-- This represents the ability to convert image URLs to ASCII art
class (Monad m) => HasAsciiConverter m where
  -- | Convert an image URL to ASCII art
  imageUrlToAscii :: Url.ImageUrl -> m (Either Text Ascii.Ascii)

