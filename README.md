# Gimlight

## Prerequisites

You need [Stack](https://docs.haskellstack.org/en/stable/).

## Build

You need to explicitly specify `LANG` to build the project due to [a bug in `c2hs`](https://github.com/haskell/c2hs/issues/238).

```bash
LANG=en.US.UTF-8 stack build
```
