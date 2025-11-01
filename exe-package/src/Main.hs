{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Except (ExceptT (..), runExceptT, withExceptT)
import Control.Monad.Reader (ReaderT (runReaderT))
import qualified Data.Text as Text

import qualified Domain.Ascii as Ascii
import qualified Domain.Pokemon as Pokemon

import Application.AppM
import Application.Ascii
import Application.Pokemon

-- | Business logic using capability type classes
-- This function works with any monad that implements the required capabilities
getAsciiImageByName ::
  (HasPokemonFetcher m, HasAsciiConverter m) =>
  Pokemon.PokemonName ->
  m (Either Pokemon.DomainError Ascii.Ascii)
getAsciiImageByName name = do
  runExceptT $ do
    pokemon <- ExceptT $ fetchPokemonByName name
    let imageUrl = Pokemon.imageUrl pokemon
    withExceptT Pokemon.ExternalApiError $
      ExceptT $
        imageUrlToAscii imageUrl

main :: IO ()
main = do
  -- Wire up dependencies: inject real implementations into environment
  let env =
        Env
          { pokemonFetcher = Application.Pokemon.fetchPokemonByNameHttp,
            asciiConverter = Application.Ascii.fetchAsciiImageByUrlHttp
          }

  -- Run the application
  result <- runReaderT (runAppM (getAsciiImageByName $ Pokemon.PokemonName "pikachu")) env
  
  case result of
    Left err -> putStrLn $ "Error: " ++ show err
    Right ascii -> putStrLn $ "ASCII Image:\n" ++ Text.unpack (Ascii.content ascii)
