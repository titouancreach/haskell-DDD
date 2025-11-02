{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Application monad and capability implementations
This module aggregates all feature capabilities and provides the production monad
-}
module Application.AppM where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ReaderT, ask, asks, runReaderT)

import qualified Domain.Ascii as Ascii
import qualified Domain.Pokemon as Pokemon
import qualified Domain.Url as Url

import Application.Ascii.Capability
import Application.Pokemon.Capability
import Colog

-- | Environment holding concrete implementations for each feature
data Env = Env
  { pokemonFetcher :: Pokemon.PokemonName -> IO (Either Pokemon.DomainError Pokemon.Pokemon)
  , asciiConverter :: Url.ImageUrl -> IO (Either Ascii.Error Ascii.Ascii)
  , logAction :: LogAction IO Message
  }

-- | Production application monad
newtype AppM a = AppM {runAppM :: ReaderT Env IO a}
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader Env)

instance HasLog Env Message AppM where
  getLogAction = do
    LogAction ioAct <- asks logAction
    return $ LogAction $ \msg -> liftIO (ioAct msg)
  setLogAction :: LogAction AppM Message -> Env -> Env
  setLogAction newLogAction env =
    let LogAction appMAct = newLogAction
        -- Convert LogAction AppM Message to LogAction IO Message
        -- by extracting the IO action using the environment
        ioAct msg = runReaderT (runAppM (appMAct msg)) env
     in env{logAction = LogAction ioAct}

-- | Implement Pokemon capability for AppM
instance HasPokemonFetcher AppM where
  fetchPokemonByName name = do
    env <- ask
    logWarning "Fetching Pokemon"
    liftIO $ pokemonFetcher env name

-- | Implement ASCII capability for AppM
instance HasAsciiConverter AppM where
  imageUrlToAscii url = do
    env <- ask
    liftIO $ asciiConverter env url
