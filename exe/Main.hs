{-# LANGUAGE OverloadedStrings #-}

module Main where

import AppM (Env (..), runAppM)
import Control.Monad.Reader (ReaderT (runReaderT))
import qualified Domain.Pokemon as D
import Infra.PokemonApiFetcher
import Network.HTTP.Req
import PokemonService ()

liveHttpClient :: HttpClient IO
liveHttpClient = HttpClient $ \url opts -> do
  result <- runReq defaultHttpConfig $ do
    response <- req GET url NoReqBody jsonResponse opts
    pure (responseBody response :: ApiPokemonResponse)
  pure (Right result)

main :: IO ()
main = do
  let env = Env liveHttpClient
  result <- runReaderT (runAppM (D.fetchPokemonByName $ D.PokemonName "pikachu")) env
  print result
