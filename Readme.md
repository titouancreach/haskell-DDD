# DDD experimenting in Haskell

- Primitive obsession (PokemonId, PokemonName)
- Define typeclass in the domain and implement it in the infrastructure
- Mock dependencies with the appM monad
- Tests using mocked dependencies

## Project Structure

This project follows a standard multi-package Cabal structure with clear separation of concerns:

```
├── domain/              # Domain layer (pure business logic)
│   ├── src/
│   │   └── Domain/
│   │       ├── Ascii.hs
│   │       ├── Pokemon.hs
│   │       └── Url.hs
│   └── domain.cabal
│
├── application/         # Application layer (use cases & services)
│   ├── src/
│   │   ├── AppM.hs
│   │   ├── AsciiImageService.hs
│   │   ├── PokemonService.hs
│   │   └── Infra/
│   │       ├── AsciiImageFetcher.hs
│   │       └── PokemonApiFetcher.hs
│   ├── test/
│   └── application.cabal
│
├── exe-package/         # Executable (entry point)
│   ├── src/
│   │   └── Main.hs
│   └── exe-package.cabal
│
└── cabal.project        # Multi-package project configuration
```

### Building and Running

```bash
# Build all packages
cabal build all

# Run the executable
cabal run DDD

# Run tests
cabal test all

# Build a specific package
cabal build domain
cabal build application
cabal build exe-package
```

TODO & ideas:
- Try things with free monads
- Domain events
- Repositories
- Logging and observability
