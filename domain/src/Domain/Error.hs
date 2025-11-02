module Domain.Error where

import Data.Text (Text)

-- | Shared kernel error type for unexpected errors
-- This is used to map all domain errors to a common type at the application boundary
data UnexpectedError = UnexpectedError Text
  deriving (Show, Eq)

