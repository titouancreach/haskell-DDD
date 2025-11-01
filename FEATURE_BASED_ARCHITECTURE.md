# Feature-Based Architecture

## Overview

The codebase has been restructured to use a **feature-based organization** while maintaining clean separation between Domain and Application layers.

## Directory Structure

```
application/src/Application/
├── Pokemon/
│   ├── Capability.hs      # Type class: HasPokemonFetcher
│   ├── Infra.hs           # HTTP implementation: fetchPokemonByNameHttp
│   └── Service.hs         # Business logic (extensible)
├── Pokemon.hs             # Re-exports all Pokemon functionality
├── Ascii/
│   ├── Capability.hs      # Type class: HasAsciiConverter
│   ├── Infra.hs           # HTTP implementation: fetchAsciiImageByUrlHttp
│   └── Service.hs         # Business logic (extensible)
├── Ascii.hs               # Re-exports all ASCII functionality
├── AppM.hs                # Production monad + capability instances
└── TestM.hs               # Test monad + mock implementations
```

## Module Organization

### Feature Modules

Each feature (Pokemon, Ascii) is organized in its own directory with:

1. **Capability.hs** - Defines the capability type class
   ```haskell
   class (Monad m) => HasPokemonFetcher m where
     fetchPokemonByName :: PokemonName -> m (Either DomainError Pokemon)
   ```

2. **Infra.hs** - Contains infrastructure implementations
   ```haskell
   fetchPokemonByNameHttp :: PokemonName -> IO (Either DomainError Pokemon)
   ```

3. **Service.hs** - Placeholder for business logic
   - Can be extended with validation, transformation, etc.

4. **Feature.hs** (at parent level) - Re-exports everything
   ```haskell
   module Application.Pokemon
     ( module Application.Pokemon.Capability
     , fetchPokemonByNameHttp
     )
   ```

### Re-Export Pattern

The re-export modules (`Application.Pokemon`, `Application.Ascii`) provide a clean API:

```haskell
-- Users can import just the feature module
import Application.Pokemon

-- Or access submodules directly
import Application.Pokemon.Capability
import Application.Pokemon.Infra
```

## Benefits

### ✅ Feature-Based Organization
- All Pokemon-related code is in `Application/Pokemon/`
- Easy to find and understand feature boundaries
- Supports TDD workflow (group by feature)

### ✅ Clean Module Exports
- Each feature has a main re-export module
- Submodules can be imported individually if needed
- Clear separation: Capability, Infra, Service

### ✅ Layered Architecture
- **Domain Layer**: Pure data types (`domain/src/Domain/`)
- **Application Layer**: Feature-based with capabilities (`application/src/Application/`)
- **Infrastructure**: Within each feature's `Infra.hs`
- **Main**: Dependency injection and wiring

### ✅ No Orphan Instances
- All capability instances in `Application.AppM` and `Application.TestM`
- Type classes defined in feature `Capability.hs` modules
- Clean, maintainable structure

## Usage Examples

### Using Feature Modules

```haskell
-- Import entire feature
import Application.Pokemon

-- Or import specific parts
import Application.Pokemon.Capability (HasPokemonFetcher, fetchPokemonByName)
import Application.Pokemon.Infra (fetchPokemonByNameHttp)
```

### Writing Business Logic

```haskell
-- Business logic uses capability constraints
getPokemonWithDetails ::
  (HasPokemonFetcher m, HasAsciiConverter m) =>
  PokemonName ->
  m Result
getPokemonWithDetails name = do
  pokemon <- fetchPokemonByName name
  -- ... use pokemon
```

### Dependency Injection in Main

```haskell
main :: IO ()
main = do
  let env = Env
        { pokemonFetcher = Application.Pokemon.fetchPokemonByNameHttp
        , asciiConverter = Application.Ascii.fetchAsciiImageByUrlHttp
        }
  result <- runReaderT (runAppM businessLogic) env
  -- ...
```

### Testing with TestM

```haskell
import Application.TestM

testEnv = TestEnv
  { mockPokemon = [mockPokemonSuccess name pokemon]
  , mockAsciiImages = [mockAsciiSuccess url ascii]
  }

(result, state) <- runTest testEnv businessLogic
```

## Adding a New Feature

To add a new feature (e.g., `User`):

1. **Create feature directory:**
   ```
   Application/User/
   ```

2. **Add Capability.hs:**
   ```haskell
   module Application.User.Capability where
   class (Monad m) => HasUserRepository m where
     getUserById :: UserId -> m (Either Error User)
   ```

3. **Add Infra.hs:**
   ```haskell
   module Application.User.Infra where
   getUserByIdHttp :: UserId -> IO (Either Error User)
   ```

4. **Add Service.hs:**
   ```haskell
   module Application.User.Service where
   -- Business logic here
   ```

5. **Add User.hs (re-export):**
   ```haskell
   module Application.User
     ( module Application.User.Capability
     , getUserByIdHttp
     )
   ```

6. **Implement capability in AppM.hs:**
   ```haskell
   instance HasUserRepository AppM where
     getUserById id = do
       env <- ask
       liftIO $ userRepository env id
   ```

7. **Update Env in AppM.hs:**
   ```haskell
   data Env = Env
     { -- ... existing fields
     , userRepository :: UserId -> IO (Either Error User)
     }
   ```

8. **Add to cabal file:**
   ```yaml
   exposed-modules:
     -- ... existing modules
     , Application.User
     , Application.User.Capability
     , Application.User.Infra
     , Application.User.Service
   ```

## Module Naming Convention

- **Feature modules**: `Application.FeatureName`
- **Capability**: `Application.FeatureName.Capability`
- **Infrastructure**: `Application.FeatureName.Infra`
- **Service**: `Application.FeatureName.Service`
- **Implementation functions**: `featureActionHttp` (suffix with `Http` for HTTP implementations)

This makes it clear:
- Where the capability is defined
- Where the implementation lives
- What the function does (HTTP suffix)

## Summary

✅ Feature-based organization for TDD workflow  
✅ Clean module exports with re-exports  
✅ Separation of Domain and Application layers  
✅ No orphan instances  
✅ Easy to extend with new features  
✅ Testable with TestM monad

