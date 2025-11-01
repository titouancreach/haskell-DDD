{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module AsciiImageService where

import qualified AppM
import Control.Monad.Reader (ask, liftIO)
import qualified Domain.Ascii as Ascii
import qualified Domain.Url as Url
import qualified Infra.AsciiImageFetcher as AsciiImageFetcher

instance Ascii.ImageUrlToAscii AppM.AppM where
  imageUrlToAscii (Url.ImageUrl url) = do
    env <- ask
    result <- liftIO $ AsciiImageFetcher.fetchAsciiImageByUrlWithClient (AppM.httpClientAscii env) url
    pure (Ascii.Ascii <$> result)
