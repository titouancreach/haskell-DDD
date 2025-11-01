{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.Hspec (Spec, describe, hspec, it, shouldBe)

import qualified Domain.Ascii as Ascii
import qualified Domain.Pokemon as Pokemon
import qualified Domain.Url as Url

import Application.AppM
import Application.Ascii.Capability
import Application.Pokemon.Capability
import Application.TestM

-- | Example business logic that we want to test
-- This is the same function from Main.hs but works with any HasPokemonFetcher/HasAsciiConverter
getAsciiImageByName ::
  (HasPokemonFetcher m, HasAsciiConverter m) =>
  Pokemon.PokemonName ->
  m (Either Pokemon.DomainError Ascii.Ascii)
getAsciiImageByName name = do
  result <- fetchPokemonByName name
  case result of
    Left err -> pure (Left err)
    Right pokemon -> do
      asciiResult <- imageUrlToAscii (Pokemon.imageUrl pokemon)
      pure $ case asciiResult of
        Left err -> Left (Pokemon.ExternalApiError err)
        Right ascii -> Right ascii

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "getAsciiImageByName with TestM" $ do
    it "returns ASCII art when Pokemon is found and conversion succeeds" $ do
      let pikachuUrl = Url.ImageUrl "https://img/pika.png"
          pikachu = Pokemon.Pokemon (Pokemon.PokemonId 25) (Pokemon.PokemonName "pikachu") 60 pikachuUrl
          asciiArt = Ascii.Ascii "ASCII art of Pikachu"
          
          testEnv = TestEnv
            { mockPokemon = [mockPokemonSuccess (Pokemon.PokemonName "pikachu") pikachu],
              mockAsciiImages = [mockAsciiSuccess pikachuUrl asciiArt]
            }
      
      (result, _state) <- runTest testEnv (getAsciiImageByName (Pokemon.PokemonName "pikachu"))
      result `shouldBe` Right asciiArt

    it "returns error when Pokemon is not found" $ do
      let testEnv = TestEnv
            { mockPokemon = [mockPokemonError (Pokemon.PokemonName "unknown") Pokemon.NotFound],
              mockAsciiImages = []
            }
      
      (result, _state) <- runTest testEnv (getAsciiImageByName (Pokemon.PokemonName "unknown"))
      result `shouldBe` Left Pokemon.NotFound

    it "tracks which functions were called during test" $ do
      let pikachuUrl = Url.ImageUrl "https://img/pika.png"
          pikachu = Pokemon.Pokemon (Pokemon.PokemonId 25) (Pokemon.PokemonName "pikachu") 60 pikachuUrl
          asciiArt = Ascii.Ascii "ASCII art of Pikachu"
          
          testEnv = TestEnv
            { mockPokemon = [mockPokemonSuccess (Pokemon.PokemonName "pikachu") pikachu],
              mockAsciiImages = [mockAsciiSuccess pikachuUrl asciiArt]
            }
      
      (_result, state) <- runTest testEnv (getAsciiImageByName (Pokemon.PokemonName "pikachu"))
      
      -- We can verify that the right functions were called
      pokemonFetchCalls state `shouldBe` [Pokemon.PokemonName "pikachu"]
      asciiConversionCalls state `shouldBe` [pikachuUrl]
