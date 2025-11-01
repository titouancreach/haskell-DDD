module Domain.Pokemon (Pokemon (..), PokemonFetcher (..), PokemonId (..), PokemonName (..), DomainError (..)) where

import Data.Text (Text)

data DomainError
  = NotFound
  | InvalidData Text
  | ExternalApiError Text
  deriving (Show, Eq)

newtype PokemonName = PokemonName {unPokemonName :: Text}
  deriving (Show, Eq, Ord)

newtype PokemonId = PokemonId {unPokemonId :: Int}
  deriving (Show, Eq, Ord)

class (Monad m) => PokemonFetcher m where
  fetchPokemonByName :: PokemonName -> m (Either DomainError Pokemon)

data Pokemon = Pokemon
  { pokemonId :: PokemonId,
    name :: PokemonName,
    height :: Int
  }
  deriving (Show, Eq)
