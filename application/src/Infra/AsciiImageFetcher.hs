{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Infra.AsciiImageFetcher where

import Data.Text (Text)
import Network.HTTP.Req (Option, Scheme (Https), Url, https, (/:), (=:))

newtype HttpClient m = HttpClient
  { getText :: Url Https -> Option Https -> m (Either Text Text)
  }

fetchAsciiImageByUrlWithClient :: HttpClient IO -> Text -> IO (Either Text Text)
fetchAsciiImageByUrlWithClient client imageUrl = do
  let url = https "api.apileague.com" /: "convert-image-to-ascii-txt"
      params :: Option Https
      params = "url" =: (imageUrl :: Text) <> "width" =: (100 :: Int) <> "height" =: (100 :: Int)

  result <- getText client url params
  case result of
    Left err -> pure $ Left err
    Right asciiImage -> pure $ Right asciiImage
