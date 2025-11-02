{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Reader (ReaderT (runReaderT))
import qualified Data.Text as Text

import qualified Domain.Ascii as Ascii
import qualified Domain.Error as Error
import qualified Domain.Pokemon as Pokemon

import Application.AppM
import Application.Ascii
import Application.Pokemon

-- | Map Pokemon domain errors to UnexpectedError
mapPokemonError :: Pokemon.DomainError -> Error.UnexpectedError
mapPokemonError (Pokemon.NotFound) = Error.UnexpectedError "Pokemon not found"
mapPokemonError (Pokemon.InvalidData msg) = Error.UnexpectedError $ "Invalid data: " <> msg
mapPokemonError (Pokemon.ExternalApiError msg) = Error.UnexpectedError $ "External API error: " <> msg

-- | Map Ascii errors to UnexpectedError
mapAsciiError :: Ascii.Error -> Error.UnexpectedError
mapAsciiError (Ascii.ExternalApiError msg) = Error.UnexpectedError $ "ASCII conversion error: " <> msg

{- | Business logic using capability type classes
This function works with any monad that implements the required capabilities
-}
getAsciiImageByName ::
  (HasPokemonFetcher m, HasAsciiConverter m) =>
  Pokemon.PokemonName ->
  m (Either Error.UnexpectedError Ascii.Ascii)
getAsciiImageByName name = do
  pokemonResult <- fetchPokemonByName name
  case pokemonResult of
    Left pokemonErr -> pure $ Left $ mapPokemonError pokemonErr
    Right pokemon -> do
      asciiResult <- imageUrlToAscii (Pokemon.imageUrl pokemon)
      pure $ case asciiResult of
        Left asciiErr -> Left $ mapAsciiError asciiErr
        Right ascii -> Right ascii

main :: IO ()
main = do
  -- Wire up dependencies: inject real implementations into environment
  let env =
        Env
          { pokemonFetcher = Application.Pokemon.fetchPokemonByNameHttp
          , asciiConverter = Application.Ascii.fetchAsciiImageByUrlHttp
          }

  -- Run the application
  result <- runReaderT (runAppM (getAsciiImageByName $ Pokemon.PokemonName "pikachu")) env

  case result of
    Left err -> putStrLn $ "Error: " ++ show err
    Right ascii -> putStrLn $ "ASCII Image:\n" ++ Text.unpack (Ascii.content ascii)
