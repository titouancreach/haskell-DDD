{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Domain.Pokemon as D
import Domain.Url (ImageUrl (ImageUrl))
import Infra.PokemonApiFetcher
import Test.Hspec

-- | A fake HttpClient that returns a mocked ApiPokemonResponse
fakeHttpClient :: HttpClient IO
fakeHttpClient = HttpClient $ \_url _opts -> do
  let apiResponse =
        ApiPokemonResponse
          { pokemonId = 25,
            name = "pikachu",
            height = 60,
            sprites = ApiPokemonSprites {front_default = "https://img/pika.png"}
          }
  pure $ Right apiResponse

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "fetchPokemonByNameWithClient" $ do
  it "parses Pikachu correctly from fake response" $ do
    result <- fetchPokemonByNameWithClient fakeHttpClient (D.PokemonName "pikachu")
    result
      `shouldBe` Right (D.Pokemon (D.PokemonId 25) (D.PokemonName "pikachu") 60 (ImageUrl "https://img/pika.png"))
