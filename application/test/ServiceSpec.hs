{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.Hspec (Spec, describe, hspec, it, shouldBe)

import qualified Domain.Pokemon as Pokemon
import Domain.Url (ImageUrl (ImageUrl))
import qualified Infra.PokemonApiFetcher as PokemonApiFetcher

-- | A fake HttpClient that returns a mocked ApiPokemonResponse
fakeHttpClient :: PokemonApiFetcher.HttpClient IO
fakeHttpClient = PokemonApiFetcher.HttpClient $ \_url _opts -> do
  let apiResponse =
        PokemonApiFetcher.ApiPokemonResponse
          { PokemonApiFetcher.pokemonId = 25,
            PokemonApiFetcher.name = "pikachu",
            PokemonApiFetcher.height = 60,
            PokemonApiFetcher.sprites = PokemonApiFetcher.ApiPokemonSprites {PokemonApiFetcher.front_default = "https://img/pika.png"}
          }
  pure $ Right apiResponse

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "fetchPokemonByNameWithClient" $ do
  it "parses Pikachu correctly from fake response" $ do
    result <- PokemonApiFetcher.fetchPokemonByNameWithClient fakeHttpClient (Pokemon.PokemonName "pikachu")
    result
      `shouldBe` Right (Pokemon.Pokemon (Pokemon.PokemonId 25) (Pokemon.PokemonName "pikachu") 60 (ImageUrl "https://img/pika.png"))
