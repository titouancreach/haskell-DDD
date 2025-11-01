{-# LANGUAGE OverloadedStrings #-}

module Main where

import AppM (AppM, Env (..), runAppM)
import AsciiImageService ()
import Control.Monad.Except (ExceptT (..), runExceptT, withExceptT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT (runReaderT))
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Domain.Ascii as D
import qualified Domain.Pokemon as D
import qualified Infra.AsciiImageFetcher as AsciiImageFetcher
import qualified Infra.PokemonApiFetcher as PokemonApiFetcher
import Network.HTTP.Req
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
    pure (responseBody response :: BS.ByteString)
  pure $ Right (TE.decodeUtf8 result)

getAsciiImageByName :: D.PokemonName -> AppM (Either D.DomainError D.Ascii)
getAsciiImageByName name = do
  result <- runExceptT $ do
    pokemon <- ExceptT $ D.fetchPokemonByName name
    let imageUrl = D.imageUrl pokemon
    withExceptT D.ExternalApiError $
      ExceptT $
        D.imageUrlToAscii imageUrl
  pure $ result

main :: IO ()
main = do
  let env = Env pokemonHttpClient asciiHttpClient
  result <- runReaderT (runAppM (getAsciiImageByName $ D.PokemonName "pikachu")) env
  case result of
    Left err -> putStrLn $ "Error: " ++ show err
    Right ascii -> putStrLn $ "ASCII Image:\n" ++ T.unpack (D.content ascii)
