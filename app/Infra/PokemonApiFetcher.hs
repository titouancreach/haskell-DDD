{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Infra.PokemonApiFetcher where

import Data.Aeson
import Data.Text (Text)
import qualified Domain.Pokemon as D
import GHC.Generics
import Network.HTTP.Req

data HttpClient m = HttpClient
  { getJson :: Url Https -> Option Https -> m (Either String ApiPokemonResponse)
  }

newtype ApiPokemonSprites = ApiPokemonSprites
  { front_default :: Maybe Text
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

fromPokemonApiResponse :: ApiPokemonResponse -> D.Pokemon
fromPokemonApiResponse apiResp =
  D.Pokemon (D.PokemonId (pokemonId apiResp)) (D.PokemonName (name apiResp)) (height apiResp)

fetchPokemonByNameWithClient ::
  HttpClient IO ->
  D.PokemonName ->
  IO (Either D.DomainError D.Pokemon)
fetchPokemonByNameWithClient client pokemonName = do
  let url = https "pokeapi.co" /: "api" /: "v2" /: "pokemon" /~ D.unPokemonName pokemonName
  result <- getJson client url mempty
  case result of
    Left _ -> pure . Left $ D.ExternalApiError (D.unPokemonName pokemonName)
    Right apiPokemon -> pure . Right $ fromPokemonApiResponse apiPokemon
