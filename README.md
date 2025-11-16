# aws-eventbridge-cron

Utilities for exploring and validating AWS EventBridge cron expressions.

Status: early draft. API may change prior to 1.0.

## What’s here

- Parsers for individual cron fields used by EventBridge (minutes, hours, day-of-month,
	day-of-week, months, years) with evaluation helpers for valid value lists.
- Parser and evaluator for EventBridge rate expressions that produce validated
	`NominalDiffTime` durations.
- Property-based test suite that exercises nominal and edge-case inputs drawn from
	the [AWS EventBridge cron expression documentation][aws-docs].
- Library code compiled with GHC2021 and warnings enabled by default.

## Usage

The library currently exposes low-level modules for each cron field. For example, you
can parse and evaluate month expressions like this:

```haskell
import AWS.EventBridge.Months (evaluateMonthT, parseMonthsText)

eitherExpr :: Either String [Int]
eitherExpr = do
	expr <- parseMonthsText "Jan,4,Aug"
	evaluateMonthT expr
-- Right [1,4,8]
```

Similar helpers exist for minutes (`AWS.EventBridge.Minutes`), hours (`AWS.EventBridge.Hours`),
day-of-month (`AWS.EventBridge.DayOfMonth`), day-of-week (`AWS.EventBridge.DayOfWeek`), years
(`AWS.EventBridge.Years`), and rate expressions (`AWS.EventBridge.Rate`). A higher-level `Cron`
module will follow once the individual field modules stabilise.

Rate expressions evaluate to `NominalDiffTime` durations:

```haskell
import AWS.EventBridge.Rate (evaluateRateT, parseRateText)
import Data.Time.Clock (NominalDiffTime)

eitherDuration :: Either String NominalDiffTime
eitherDuration = do
	expr <- parseRateText "rate(5 minutes)"
	evaluateRateT expr
-- Right 300s
```

## Developing

- Requires GHC and cabal-install.
- GHC2021 is the default language.

```bash
cabal build
cabal repl
cabal test
```

## Contributing

Bug reports, suggestions, and pull requests are welcome. Please file issues or share ideas
before large-scale changes to ensure we keep the API coherent.

## License

BSD-3-Clause

[aws-docs]: https://docs.aws.amazon.com/eventbridge/latest/userguide/eb-scheduled-rule-pattern.html#eb-cron-expressions