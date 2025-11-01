{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module AsciiImageService where

import qualified AppM as A
import Control.Monad.Reader (ask, liftIO)
import Data.Aeson
import Data.Text (Text)
import qualified Domain.Ascii as D
import qualified Domain.Pokemon as D
import qualified Domain.Url as Url
import GHC.Generics
import qualified Infra.AsciiImageFetcher as AsciiImageFetcher
import Network.HTTP.Req

instance D.ImageUrlToAscii A.AppM where
  imageUrlToAscii (Url.ImageUrl url) = do
    env <- ask
    result <- liftIO $ AsciiImageFetcher.fetchAsciiImageByUrlWithClient (A.httpClientAscii env) url
    pure (D.Ascii <$> result)
