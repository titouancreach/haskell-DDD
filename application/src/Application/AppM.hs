{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Application monad and capability implementations
-- This module aggregates all feature capabilities and provides the production monad
module Application.AppM where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ReaderT, ask)
import Data.Text (Text)

import qualified Domain.Ascii as Ascii
import qualified Domain.Pokemon as Pokemon
import qualified Domain.Url as Url

import Application.Ascii.Capability
import Application.Pokemon.Capability

-- | Environment holding concrete implementations for each feature
data Env = Env
  { pokemonFetcher :: Pokemon.PokemonName -> IO (Either Pokemon.DomainError Pokemon.Pokemon),
    asciiConverter :: Url.ImageUrl -> IO (Either Text Ascii.Ascii)
  }

-- | Production application monad
newtype AppM a = AppM {runAppM :: ReaderT Env IO a}
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader Env)

-- | Implement Pokemon capability for AppM
instance HasPokemonFetcher AppM where
  fetchPokemonByName name = do
    env <- ask
    liftIO $ pokemonFetcher env name

-- | Implement ASCII capability for AppM
instance HasAsciiConverter AppM where
  imageUrlToAscii url = do
    env <- ask
    liftIO $ asciiConverter env url

