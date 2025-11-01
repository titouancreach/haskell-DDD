-- | Pokemon feature module
-- This module re-exports all Pokemon-related functionality
module Application.Pokemon
  ( -- Re-export capability (type class)
    module Application.Pokemon.Capability,
    -- Re-export infrastructure implementation
    fetchPokemonByNameHttp,
  )
where

import Application.Pokemon.Capability
import Application.Pokemon.Infra (fetchPokemonByNameHttp)
