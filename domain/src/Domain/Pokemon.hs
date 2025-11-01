module Domain.Pokemon (Pokemon (..), PokemonId (..), PokemonName (..), DomainError (..)) where

import Data.Text (Text)
import qualified Domain.Url as Url

data DomainError
  = NotFound
  | InvalidData Text
  | ExternalApiError Text
  deriving (Show, Eq)

newtype PokemonName = PokemonName {unPokemonName :: Text}
  deriving (Show, Eq, Ord)

newtype PokemonId = PokemonId {unPokemonId :: Int}
  deriving (Show, Eq, Ord)

data Pokemon = Pokemon
  { pokemonId :: PokemonId
  , name :: PokemonName
  , height :: Int
  , imageUrl :: Url.ImageUrl
  }
  deriving (Show, Eq)
