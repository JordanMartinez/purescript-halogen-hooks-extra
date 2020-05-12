# Purescript-Halogen-Hooks-Extra

Provides useful Halogen Hooks and other utilities commonly used in hooks (e.g. actions).

## Installation via Spago

Update your `package.dhall` file to the following until `halogen-hooks` `v0.3.0` is included in the package set.
```dhall
let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.13.6-20200507/packages.dhall sha256:9c1e8951e721b79de1de551f31ecb5a339e82bbd43300eb5ccfb1bf8cf7bbd62

let overrides =
  { halogen-hooks = upstream.halogen-hooks // { version = "v0.3.0" }
  }

let additions = {=}

in  upstream // overrides // additions
```

## Examples

```bash
spago -x examples.dhall build
```
