{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Infra.PokemonApiFetcher where

import Control.Exception (try, SomeException)
import Data.Aeson (FromJSON, parseJSON, withObject, (.:))
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)
import Network.HTTP.Req (
  GET (GET),
  NoReqBody (NoReqBody),
  Option,
  Scheme (Https),
  defaultHttpConfig,
  https,
  jsonResponse,
  req,
  responseBody,
  runReq,
  (/:),
  (/~),
 )

import qualified Domain.Pokemon as Pokemon
import qualified Domain.Url as Url

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

-- Pure implementation that can be injected
fetchPokemonByName ::
  Pokemon.PokemonName ->
  IO (Either Pokemon.DomainError Pokemon.Pokemon)
fetchPokemonByName pokemonName = do
  let url = https "pokeapi.co" /: "api" /: "v2" /: "pokemon" /~ Pokemon.unPokemonName pokemonName
  result <- try $ runReq defaultHttpConfig $ do
    response <- req GET url NoReqBody jsonResponse (mempty :: Option Https)
    pure (responseBody response :: ApiPokemonResponse)
  case result of
    Left (err :: SomeException) -> 
      pure . Left $ Pokemon.ExternalApiError (Text.pack $ show err)
    Right apiPokemon -> 
      pure . Right $ fromPokemonApiResponse apiPokemon
