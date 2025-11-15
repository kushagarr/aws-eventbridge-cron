# aws-eventbridge-cron

Utilities for working with AWS EventBridge cron and rate expressions.

Status: early draft. API may change prior to 1.0.

## Usage

```haskell
import AWS.EventBridge.Cron

expr1 = render (cron "0 12 * * ? *")      -- cron(0 12 * * ? *)
expr2 = render (rateMinutes 5)             -- rate(5 minutes)
```

## Developing

- Requires GHC and cabal-install.
- GHC2021 is the default language.

```bash
cabal build
cabal repl
cabal test
```

## License

BSD-3-Clause