{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module PokemonService where

import Control.Monad.Reader (ask, liftIO)

import qualified AppM
import qualified Domain.Pokemon as Pokemon
import qualified Infra.PokemonApiFetcher as PokemonApiFetcher

instance Pokemon.PokemonFetcher AppM.AppM where
  fetchPokemonByName name = do
    env <- ask
    liftIO $ PokemonApiFetcher.fetchPokemonByNameWithClient (AppM.httpClientPokemon env) name
