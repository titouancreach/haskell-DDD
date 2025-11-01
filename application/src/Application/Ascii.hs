-- | Ascii feature module
-- This module re-exports all ASCII-related functionality
module Application.Ascii
  ( -- Re-export capability (type class)
    module Application.Ascii.Capability,
    -- Re-export infrastructure implementation
    fetchAsciiImageByUrlHttp,
  )
where

import Application.Ascii.Capability
import Application.Ascii.Infra (fetchAsciiImageByUrlHttp)
