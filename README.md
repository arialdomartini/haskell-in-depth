Haskell In Depth
================

Exercises reading the Haskell In Depth book.

## Project set up

The following is the step with which the project has been created:

```bash
cabal init --tests --test-dir=tests --non-interactive
```

## Running tests
```bash
cabal test
```

For a pretty-printed output, use:

```bash
cabal test --test-show-details=direct --verbose=0
```

or run

```bash
./test.sh
```

The test suite can be built with:

```bash
cabal build haskell-in-depth-test
```