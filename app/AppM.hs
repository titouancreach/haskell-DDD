{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module AppM where

import Control.Monad.IO.Class
import Control.Monad.Reader
import Infra.PokemonApiFetcher (HttpClient)

data Env = Env
  { httpClient :: HttpClient IO
  }

newtype AppM a = AppM {runAppM :: ReaderT Env IO a}
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader Env)
