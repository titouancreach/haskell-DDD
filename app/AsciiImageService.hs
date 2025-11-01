{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module AsciiImageService where

import Control.Monad.Reader (ask, liftIO)

import qualified AppM
import qualified Domain.Ascii as Ascii
import qualified Domain.Url as Url
import qualified Infra.AsciiImageFetcher as AsciiImageFetcher

instance Ascii.ImageUrlToAscii AppM.AppM where
  imageUrlToAscii (Url.ImageUrl url) = do
    env <- ask
    result <- liftIO $ AsciiImageFetcher.fetchAsciiImageByUrlWithClient (AppM.httpClientAscii env) url
    pure (Ascii.Ascii <$> result)
