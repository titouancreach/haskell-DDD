{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module TestM where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, ReaderT, ask, runReaderT)
import Control.Monad.State (MonadState, StateT, modify, runStateT)
import Data.Text (Text)

import qualified AppM
import qualified Domain.Ascii as Ascii
import qualified Domain.Pokemon as Pokemon
import qualified Domain.Url as Url

-- Test environment that can be configured with mock data
data TestEnv = TestEnv
  { mockPokemon :: [(Pokemon.PokemonName, Either Pokemon.DomainError Pokemon.Pokemon)],
    mockAsciiImages :: [(Url.ImageUrl, Either Text Ascii.Ascii)]
  }

-- Test state to track calls made during tests
data TestState = TestState
  { pokemonFetchCalls :: [Pokemon.PokemonName],
    asciiConversionCalls :: [Url.ImageUrl]
  }

-- Test monad with state tracking
newtype TestM a = TestM {runTestM :: StateT TestState (ReaderT TestEnv IO) a}
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader TestEnv, MonadState TestState)

-- Implement capabilities for TestM with mock implementations
instance AppM.MonadPokemonFetcher TestM where
  fetchPokemonByName name = do
    -- Track that this call was made
    modify $ \s -> s {pokemonFetchCalls = name : pokemonFetchCalls s}
    
    -- Look up mock response
    env <- ask
    let result = lookup name (mockPokemon env)
    pure $ case result of
      Just response -> response
      Nothing -> Left $ Pokemon.NotFound

instance AppM.MonadAsciiConverter TestM where
  imageUrlToAscii url = do
    -- Track that this call was made
    modify $ \s -> s {asciiConversionCalls = url : asciiConversionCalls s}
    
    -- Look up mock response
    env <- ask
    let result = lookup url (mockAsciiImages env)
    pure $ case result of
      Just response -> response
      Nothing -> Left ("Image not found in mock data" :: Text)

-- Helper to run tests with mock data
runTest :: TestEnv -> TestM a -> IO (a, TestState)
runTest env (TestM action) = do
  let initialState = TestState {pokemonFetchCalls = [], asciiConversionCalls = []}
  runReaderT (runStateT action initialState) env

-- Example test helper: create a successful Pokemon mock
mockPokemonSuccess :: Pokemon.PokemonName -> Pokemon.Pokemon -> (Pokemon.PokemonName, Either Pokemon.DomainError Pokemon.Pokemon)
mockPokemonSuccess name pokemon = (name, Right pokemon)

-- Example test helper: create a failed Pokemon mock
mockPokemonError :: Pokemon.PokemonName -> Pokemon.DomainError -> (Pokemon.PokemonName, Either Pokemon.DomainError Pokemon.Pokemon)
mockPokemonError name err = (name, Left err)

-- Example test helper: create a successful ASCII conversion mock
mockAsciiSuccess :: Url.ImageUrl -> Ascii.Ascii -> (Url.ImageUrl, Either Text Ascii.Ascii)
mockAsciiSuccess url ascii = (url, Right ascii)

