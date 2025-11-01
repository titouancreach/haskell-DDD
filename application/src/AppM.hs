{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module AppM where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ReaderT, ask)
import Data.Text (Text)

import qualified Domain.Ascii as Ascii
import qualified Domain.Pokemon as Pokemon
import qualified Domain.Url as Url

-- Capability type classes for application layer
class (Monad m) => MonadPokemonFetcher m where
  fetchPokemonByName :: Pokemon.PokemonName -> m (Either Pokemon.DomainError Pokemon.Pokemon)

class (Monad m) => MonadAsciiConverter m where
  imageUrlToAscii :: Url.ImageUrl -> m (Either Text Ascii.Ascii)

-- Environment holds concrete implementations
data Env = Env
  { pokemonFetcher :: Pokemon.PokemonName -> IO (Either Pokemon.DomainError Pokemon.Pokemon),
    asciiConverter :: Url.ImageUrl -> IO (Either Text Ascii.Ascii)
  }

-- Application monad
newtype AppM a = AppM {runAppM :: ReaderT Env IO a}
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader Env)

-- Implement capabilities for AppM (NO orphan instances!)
instance MonadPokemonFetcher AppM where
  fetchPokemonByName name = do
    env <- ask
    liftIO $ pokemonFetcher env name

instance MonadAsciiConverter AppM where
  imageUrlToAscii url = do
    env <- ask
    liftIO $ asciiConverter env url
