{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module AppM where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, ReaderT)

import qualified Infra.AsciiImageFetcher as AsciiImageFetcher
import qualified Infra.PokemonApiFetcher as PokemonApiFetcher

data Env = Env
  { httpClientPokemon :: PokemonApiFetcher.HttpClient IO,
    httpClientAscii :: AsciiImageFetcher.HttpClient IO
  }

newtype AppM a = AppM {runAppM :: ReaderT Env IO a}
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader Env)
