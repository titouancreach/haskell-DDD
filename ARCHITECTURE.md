# Architecture Documentation

## Dependency Injection - MTL Style

This project now uses the **MTL-style** dependency injection pattern, which is the standard approach in the Haskell community.

## Layer Structure

### 1. Domain Layer (Pure)
**Location:** `domain/src/Domain/`

The domain layer contains only **pure data types** and business logic. It has no type classes for effects.

- `Domain.Pokemon` - Pokemon data types
- `Domain.Ascii` - ASCII art data types  
- `Domain.Url` - URL wrapper types
- `Domain.DomainError` - Domain-level error types

**Key principle:** The domain is effect-free and doesn't know about IO, HTTP, or any infrastructure concerns.

### 2. Application Layer (Capabilities)
**Location:** `application/src/`

The application layer defines **capability type classes** that describe what effects the application needs:

#### Capability Type Classes (`AppM.hs`)

```haskell
class (Monad m) => MonadPokemonFetcher m where
  fetchPokemonByName :: Pokemon.PokemonName -> m (Either Pokemon.DomainError Pokemon.Pokemon)

class (Monad m) => MonadAsciiConverter m where
  imageUrlToAscii :: Url.ImageUrl -> m (Either Text Ascii.Ascii)
```

#### Production Monad (`AppM.hs`)

```haskell
data Env = Env
  { pokemonFetcher :: Pokemon.PokemonName -> IO (Either Pokemon.DomainError Pokemon.Pokemon),
    asciiConverter :: Url.ImageUrl -> IO (Either Text Ascii.Ascii)
  }

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
```

#### Test Monad (`TestM.hs`)

The test monad implements the same capability interfaces but with mock data:

```haskell
data TestEnv = TestEnv
  { mockPokemon :: [(Pokemon.PokemonName, Either Pokemon.DomainError Pokemon.Pokemon)],
    mockAsciiImages :: [(Url.ImageUrl, Either Text Ascii.Ascii)]
  }

newtype TestM a = TestM {runTestM :: StateT TestState (ReaderT TestEnv IO) a}

-- Implements MonadPokemonFetcher and MonadAsciiConverter using mock data
```

### 3. Infrastructure Layer
**Location:** `application/src/Infra/`

Infrastructure provides concrete implementations that can be injected:

- `Infra.PokemonApiFetcher` - HTTP calls to Pokemon API
- `Infra.AsciiImageFetcher` - HTTP calls to ASCII art API

These are **pure functions** that return `IO` actions:

```haskell
fetchPokemonByName :: Pokemon.PokemonName -> IO (Either Pokemon.DomainError Pokemon.Pokemon)
fetchAsciiImageByUrl :: Url.ImageUrl -> IO (Either Text Ascii.Ascii)
```

### 4. Executable Layer
**Location:** `exe-package/src/Main.hs`

The main function wires everything together:

1. Creates concrete infrastructure implementations
2. Injects them into the `Env`
3. Runs business logic using capability constraints
4. Executes in the `AppM` monad

```haskell
main :: IO ()
main = do
  -- Wire up dependencies: inject real implementations
  let env = AppM.Env
        { AppM.pokemonFetcher = PokemonApiFetcher.fetchPokemonByName,
          AppM.asciiConverter = AsciiImageFetcher.fetchAsciiImageByUrl
        }
  
  -- Run the application
  result <- runReaderT (AppM.runAppM (getAsciiImageByName $ Pokemon.PokemonName "pikachu")) env
  -- ... handle result
```

## Benefits of This Architecture

### ✅ No Orphan Instances
All type class instances are defined in the same module as either the type class or the type, preventing orphan instance issues.

### ✅ Easy Testing
Business logic is written against capability interfaces, making it easy to test with `TestM`:

```haskell
getAsciiImageByName ::
  (AppM.MonadPokemonFetcher m, AppM.MonadAsciiConverter m) =>
  Pokemon.PokemonName ->
  m (Either Pokemon.DomainError Ascii.Ascii)
```

This function works with **both** `AppM` (production) and `TestM` (tests)!

### ✅ Clear Separation of Concerns
- **Domain**: Pure business logic and data types
- **Application**: Effect capabilities and monad implementations
- **Infrastructure**: Concrete implementations (HTTP, Database, etc.)
- **Executable**: Wiring and composition

### ✅ Type-Safe Dependency Injection
The compiler ensures all capabilities are provided and correctly implemented.

### ✅ Testable Without Mocking Libraries
The test monad provides a built-in way to inject test data without external mocking frameworks.

## Example: Writing Business Logic

```haskell
-- This function works with ANY monad that implements the required capabilities
processUserRequest ::
  (MonadPokemonFetcher m, MonadAsciiConverter m) =>
  Pokemon.PokemonName ->
  m (Either Pokemon.DomainError Ascii.Ascii)
processUserRequest name = do
  pokemon <- fetchPokemonByName name  -- Uses capability
  case pokemon of
    Left err -> pure (Left err)
    Right p -> do
      ascii <- imageUrlToAscii (Pokemon.imageUrl p)  -- Uses capability
      case ascii of
        Left err -> pure (Left (Pokemon.ExternalApiError err))
        Right art -> pure (Right art)
```

## Example: Writing Tests

```haskell
spec :: Spec
spec = describe "processUserRequest" $ do
  it "returns ASCII art when Pokemon is found" $ do
    let pikachu = Pokemon.Pokemon {...}
        asciiArt = Ascii.Ascii "..."
        
        testEnv = TestM.TestEnv
          { TestM.mockPokemon = [TestM.mockPokemonSuccess (Pokemon.PokemonName "pikachu") pikachu],
            TestM.mockAsciiImages = [TestM.mockAsciiSuccess url asciiArt]
          }
    
    (result, _state) <- TestM.runTest testEnv (processUserRequest (Pokemon.PokemonName "pikachu"))
    result `shouldBe` Right asciiArt
```

## Comparison: Before vs After

### Before (Problems)
- ❌ Orphan instances in `PokemonService.hs` and `AsciiImageService.hs`
- ❌ Type classes in domain layer (mixing effects with business logic)
- ❌ Multiple duplicate `HttpClient` types
- ❌ Hard to test without affecting production code

### After (Solutions)
- ✅ All instances defined in `AppM.hs` (no orphans)
- ✅ Pure domain layer
- ✅ Capability type classes in application layer
- ✅ Easy testing with `TestM`
- ✅ Standard MTL-style pattern

## Further Reading

- [MTL Style](https://www.fpcomplete.com/haskell/tutorial/monad-transformers/)
- [Capability Pattern](https://www.tweag.io/blog/2018-10-04-capability/)
- [Three Layer Haskell Cake](https://www.parsonsmatt.org/2018/03/22/three_layer_haskell_cake.html)

