{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Except (ExceptT (..), runExceptT, withExceptT)
import Control.Monad.Reader (ReaderT (runReaderT))
import qualified Data.Text as Text

import qualified AppM
import qualified Domain.Ascii as Ascii
import qualified Domain.Pokemon as Pokemon
import qualified Infra.AsciiImageFetcher as AsciiImageFetcher
import qualified Infra.PokemonApiFetcher as PokemonApiFetcher

-- Business logic using capability type classes
getAsciiImageByName ::
  (AppM.MonadPokemonFetcher m, AppM.MonadAsciiConverter m) =>
  Pokemon.PokemonName ->
  m (Either Pokemon.DomainError Ascii.Ascii)
getAsciiImageByName name = do
  runExceptT $ do
    pokemon <- ExceptT $ AppM.fetchPokemonByName name
    let imageUrl = Pokemon.imageUrl pokemon
    withExceptT Pokemon.ExternalApiError $
      ExceptT $
        AppM.imageUrlToAscii imageUrl

main :: IO ()
main = do
  -- Wire up dependencies: inject real implementations into environment
  let env =
        AppM.Env
          { AppM.pokemonFetcher = PokemonApiFetcher.fetchPokemonByName,
            AppM.asciiConverter = AsciiImageFetcher.fetchAsciiImageByUrl
          }

  -- Run the application
  result <- runReaderT (AppM.runAppM (getAsciiImageByName $ Pokemon.PokemonName "pikachu")) env
  
  case result of
    Left err -> putStrLn $ "Error: " ++ show err
    Right ascii -> putStrLn $ "ASCII Image:\n" ++ Text.unpack (Ascii.content ascii)
