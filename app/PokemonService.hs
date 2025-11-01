{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module PokemonService where

import AppM
import Control.Monad.Reader (ask, liftIO)
import qualified Domain.Pokemon as D
import Infra.PokemonApiFetcher (fetchPokemonByNameWithClient)

instance D.PokemonFetcher AppM where
  fetchPokemonByName name = do
    env <- ask
    liftIO $ fetchPokemonByNameWithClient (httpClient env) name
