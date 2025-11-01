{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Except (ExceptT (..), runExceptT, withExceptT)
import Control.Monad.Reader (ReaderT (runReaderT))
import qualified Data.ByteString as ByteString
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Network.HTTP.Req (
  GET (GET),
  NoReqBody (NoReqBody),
  bsResponse,
  defaultHttpConfig,
  jsonResponse,
  req,
  responseBody,
  runReq,
 )

import qualified AppM
import AsciiImageService ()
import qualified Domain.Ascii as Ascii
import qualified Domain.Pokemon as Pokemon
import qualified Infra.AsciiImageFetcher as AsciiImageFetcher
import qualified Infra.PokemonApiFetcher as PokemonApiFetcher
import PokemonService ()

pokemonHttpClient :: PokemonApiFetcher.HttpClient IO
pokemonHttpClient = PokemonApiFetcher.HttpClient $ \url opts -> do
  result <- runReq defaultHttpConfig $ do
    response <- req GET url NoReqBody jsonResponse opts
    pure (responseBody response :: PokemonApiFetcher.ApiPokemonResponse)
  pure (Right result)

asciiHttpClient :: AsciiImageFetcher.HttpClient IO
asciiHttpClient = AsciiImageFetcher.HttpClient $ \url opts -> do
  result <- runReq defaultHttpConfig $ do
    response <- req GET url NoReqBody bsResponse opts
    pure (responseBody response :: ByteString.ByteString)
  pure $ Right (TextEncoding.decodeUtf8 result)

getAsciiImageByName :: Pokemon.PokemonName -> AppM.AppM (Either Pokemon.DomainError Ascii.Ascii)
getAsciiImageByName name = do
  runExceptT $ do
    pokemon <- ExceptT $ Pokemon.fetchPokemonByName name
    let imageUrl = Pokemon.imageUrl pokemon
    withExceptT Pokemon.ExternalApiError $
      ExceptT $
        Ascii.imageUrlToAscii imageUrl

main :: IO ()
main = do
  let env = AppM.Env pokemonHttpClient asciiHttpClient
  result <- runReaderT (AppM.runAppM (getAsciiImageByName $ Pokemon.PokemonName "pikachu")) env
  case result of
    Left err -> putStrLn $ "Error: " ++ show err
    Right ascii -> putStrLn $ "ASCII Image:\n" ++ Text.unpack (Ascii.content ascii)
