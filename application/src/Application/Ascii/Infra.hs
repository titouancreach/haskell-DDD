{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Application.Ascii.Infra where

import Control.Exception (SomeException, try)
import qualified Data.ByteString as ByteString
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Network.HTTP.Req (
  GET (GET),
  NoReqBody (NoReqBody),
  Option,
  Scheme (Https),
  bsResponse,
  defaultHttpConfig,
  https,
  req,
  responseBody,
  runReq,
  (/:),
  (=:),
 )

import qualified Domain.Ascii as Ascii
import qualified Domain.Url as Url

-- | HTTP implementation for converting images to ASCII
fetchAsciiImageByUrlHttp :: Url.ImageUrl -> IO (Either Ascii.Error Ascii.Ascii)
fetchAsciiImageByUrlHttp (Url.ImageUrl imageUrl) = do
  let url = https "api.apileague.com" /: "convert-image-to-ascii-txt"
      params :: Option Https
      params = "url" =: (imageUrl :: Text) <> "width" =: (100 :: Int) <> "height" =: (100 :: Int)

  result <- try $ runReq defaultHttpConfig $ do
    response <- req GET url NoReqBody bsResponse params
    pure (responseBody response :: ByteString.ByteString)

  case result of
    Left (err :: SomeException) ->
      pure $ Left (Ascii.ExternalApiError (Text.pack $ show err))
    Right bytes ->
      pure $ Right (Ascii.Ascii (TextEncoding.decodeUtf8 bytes))
