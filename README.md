# aws-eventbridge-cron

Utilities for exploring and validating AWS EventBridge cron expressions.

Status: early draft. API may change prior to 1.0.

## What’s here

- Parsers for individual cron fields used by EventBridge (minutes, hours, day-of-month,
	day-of-week, months, years) with evaluation helpers for valid value lists.
- Parser and evaluator for EventBridge rate expressions that produce validated
	`NominalDiffTime` durations.
- Parser for one-time `at(...)` expressions that yields validated `UTCTime`
	timestamps.
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
(`AWS.EventBridge.Years`), rate expressions (`AWS.EventBridge.Rate`), and one-time schedules
(`AWS.EventBridge.OneTime`). A higher-level `Cron` module will follow once the individual field
modules stabilise.

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

One-time expressions evaluate to `UTCTime` values ready for scheduling:

```haskell
import AWS.EventBridge.OneTime (evaluateOneTimeT, parseOneTimeText)
import Data.Time (UTCTime)

eitherMoment :: Either String UTCTime
eitherMoment = do
	expr <- parseOneTimeText "at(2025-11-16T09:30:00)"
	pure (evaluateOneTimeT expr)
-- Right 2025-11-16 09:30:00 UTC
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