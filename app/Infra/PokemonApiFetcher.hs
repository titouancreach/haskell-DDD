{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Infra.PokemonApiFetcher where

import Data.Aeson (FromJSON, parseJSON, withObject, (.:))
import Data.Text (Text)
import GHC.Generics (Generic)
import Network.HTTP.Req (Option, Scheme (Https), Url, https, (/:), (/~))

import qualified Domain.Pokemon as Pokemon
import qualified Domain.Url as Url

newtype HttpClient m = HttpClient
  { getJson :: Url Https -> Option Https -> m (Either String ApiPokemonResponse)
  }

newtype ApiPokemonSprites = ApiPokemonSprites
  { front_default :: Text
  }
  deriving (Show, Generic)

data ApiPokemonResponse = ApiPokemonResponse
  { pokemonId :: Int,
    name :: Text,
    height :: Int,
    sprites :: ApiPokemonSprites
  }
  deriving (Show, Generic)

instance FromJSON ApiPokemonResponse where
  parseJSON = withObject "ApiPokemon" $ \v ->
    ApiPokemonResponse
      <$> v .: "id"
      <*> v .: "name"
      <*> v .: "weight"
      <*> v .: "sprites"

instance FromJSON ApiPokemonSprites

fromPokemonApiResponse :: ApiPokemonResponse -> Pokemon.Pokemon
fromPokemonApiResponse apiResp =
  Pokemon.Pokemon
    (Pokemon.PokemonId (pokemonId apiResp))
    (Pokemon.PokemonName (name apiResp))
    (height apiResp)
    ((Url.ImageUrl . front_default . sprites) apiResp)

fetchPokemonByNameWithClient ::
  HttpClient IO ->
  Pokemon.PokemonName ->
  IO (Either Pokemon.DomainError Pokemon.Pokemon)
fetchPokemonByNameWithClient client pokemonName = do
  let url = https "pokeapi.co" /: "api" /: "v2" /: "pokemon" /~ Pokemon.unPokemonName pokemonName
  result <- getJson client url mempty
  case result of
    Left _ -> pure . Left $ Pokemon.ExternalApiError (Pokemon.unPokemonName pokemonName)
    Right apiPokemon -> pure . Right $ fromPokemonApiResponse apiPokemon
