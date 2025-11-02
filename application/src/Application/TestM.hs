{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Test monad with mock implementations
This module provides a test monad that implements all application capabilities with mock data
-}
module Application.TestM where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, ReaderT, ask, runReaderT)
import Control.Monad.State (MonadState, StateT, modify, runStateT)

import qualified Domain.Ascii as Ascii
import qualified Domain.Pokemon as Pokemon
import qualified Domain.Url as Url

import Application.Ascii.Capability
import Application.Pokemon.Capability

-- | Test environment that can be configured with mock data
data TestEnv = TestEnv
  { mockPokemon :: [(Pokemon.PokemonName, Either Pokemon.DomainError Pokemon.Pokemon)]
  , mockAsciiImages :: [(Url.ImageUrl, Either Ascii.Error Ascii.Ascii)]
  }

-- | Test state to track calls made during tests
data TestState = TestState
  { pokemonFetchCalls :: [Pokemon.PokemonName]
  , asciiConversionCalls :: [Url.ImageUrl]
  }

-- | Test monad with state tracking
newtype TestM a = TestM {runTestM :: StateT TestState (ReaderT TestEnv IO) a}
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader TestEnv, MonadState TestState)

-- | Implement Pokemon capability for TestM with mock implementations
instance HasPokemonFetcher TestM where
  fetchPokemonByName name = do
    -- Track that this call was made
    modify $ \s -> s{pokemonFetchCalls = name : pokemonFetchCalls s}

    -- Look up mock response
    env <- ask
    let result = lookup name (mockPokemon env)
    pure $ case result of
      Just response -> response
      Nothing -> Left $ Pokemon.NotFound

-- | Implement ASCII capability for TestM with mock implementations
instance HasAsciiConverter TestM where
  imageUrlToAscii url = do
    -- Track that this call was made
    modify $ \s -> s{asciiConversionCalls = url : asciiConversionCalls s}

    -- Look up mock response
    env <- ask
    let result = lookup url (mockAsciiImages env)
    pure $ case result of
      Just response -> response
      Nothing -> Left (Ascii.ExternalApiError "Image not found in mock data")

-- | Helper to run tests with mock data
runTest :: TestEnv -> TestM a -> IO (a, TestState)
runTest env (TestM action) = do
  let initialState = TestState{pokemonFetchCalls = [], asciiConversionCalls = []}
  runReaderT (runStateT action initialState) env

-- | Test helper: create a successful Pokemon mock
mockPokemonSuccess :: Pokemon.PokemonName -> Pokemon.Pokemon -> (Pokemon.PokemonName, Either Pokemon.DomainError Pokemon.Pokemon)
mockPokemonSuccess name pokemon = (name, Right pokemon)

-- | Test helper: create a failed Pokemon mock
mockPokemonError :: Pokemon.PokemonName -> Pokemon.DomainError -> (Pokemon.PokemonName, Either Pokemon.DomainError Pokemon.Pokemon)
mockPokemonError name err = (name, Left err)

-- | Test helper: create a successful ASCII conversion mock
mockAsciiSuccess :: Url.ImageUrl -> Ascii.Ascii -> (Url.ImageUrl, Either Ascii.Error Ascii.Ascii)
mockAsciiSuccess url ascii = (url, Right ascii)
