{-# LANGUAGE MultiParamTypeClasses #-}

module Application.Pokemon.Capability where

import qualified Domain.Pokemon as Pokemon

-- | Capability type class for fetching Pokemon
-- This represents the ability to fetch Pokemon data
class (Monad m) => HasPokemonFetcher m where
  -- | Fetch a Pokemon by its name
  fetchPokemonByName :: Pokemon.PokemonName -> m (Either Pokemon.DomainError Pokemon.Pokemon)

