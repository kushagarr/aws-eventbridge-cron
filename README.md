# aws-eventbridge-cron

Parse AWS EventBridge cron, rate, and one-time expressions and compute their
future run times.

Status: early preview. Expect small API tweaks before a `1.0.0` release.

## Features

- Single entry point: `AWS.EventBridge.Cron` exports the `CronExprT` type,
  `parseCronText` parser, and `nextRunTimes` scheduler.
- Full support for EventBridge-specific syntax such as `?` wildcards, `L`, `LW`,
  weekday ranges, and nth-weekday modifiers (`2#1`).
- `rate(...)` and `at(...)` expressions share the same API, so callers do not
  need to branch on expression variants.
- Extensive property-based test suite that mirrors the behaviour documented by
  AWS.

## Installation

```
cabal install aws-eventbridge-cron
```

Or add the package to your component:

```cabal
build-depends:
    aws-eventbridge-cron >= 0.1 && < 0.2
```

## Quick Start

```haskell
import AWS.EventBridge.Cron
import Data.Time (UTCTime(..), fromGregorian)
import Data.Time.LocalTime (TimeOfDay(..), timeOfDayToTime)

base :: UTCTime
base = UTCTime (fromGregorian 2025 11 16) (timeOfDayToTime (TimeOfDay 9 0 0))

example :: Either String [UTCTime]
example = do
  expr <- parseCronText "cron(0/15 9 ? NOV SUN 2025)"
  nextRunTimes expr base 4
-- Right [2025-11-16 09:00:00 UTC, 2025-11-16 09:15:00 UTC, ...]
```

The parser also accepts `rate(...)` and `at(...)` expressions:

```haskell
rateExample :: Either String [UTCTime]
rateExample = do
  expr <- parseCronText "rate(10 minutes)"
  nextRunTimes expr base 3

atExample :: Either String [UTCTime]
atExample = do
  expr <- parseCronText "at(2025-11-16T09:30:00)"
  nextRunTimes expr base 5
```

### Error Reporting

Parser and evaluator failures return `Left String` with human-readable error
messages:

```haskell
Left "day-of-month and day-of-week fields must use '?' in exactly one position"
```

The messages mirror the constraints enforced by EventBridge when you create
scheduled rules.

## Design Notes

- `CronExprT` is intentionally opaque. Construct values with `parseCronText` and
  feed them into `nextRunTimes`.
- Scheduling honours the EventBridge rule that exactly one of day-of-month or
  day-of-week must be `?`.
- Results are monotonic, capped at the requested limit, and never fall before
  the supplied base time.

See `test/AWS/EventBridge/CronSpec.hs` for more examples and edge cases.

## Development

```bash
cabal build
cabal test
cabal haddock --open
```

## Contributing

Bug reports, suggestions, and pull requests are welcome. Please open an issue
before large-scale changes so we can keep the API coherent.

## License

Released under the BSD-3-Clause license. See `LICENSE` for details.

## References

- [AWS EventBridge cron expression documentation][aws-docs]

[aws-docs]: https://docs.aws.amazon.com/scheduler/latest/UserGuide/schedule-types.html#cron-based