{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module AppM where

import Control.Monad.IO.Class
import Control.Monad.Reader
import Domain.Pokemon (PokemonFetcher)
import qualified Infra.AsciiImageFetcher as AsciiImageFetcher
import qualified Infra.PokemonApiFetcher as PokemonFetcher

data Env = Env
  { httpClientPokemon :: PokemonFetcher.HttpClient IO,
    httpClientAscii :: AsciiImageFetcher.HttpClient IO
  }

newtype AppM a = AppM {runAppM :: ReaderT Env IO a}
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader Env)
