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
- Schedule introspection helpers: `scheduleKind` returns a `ScheduleKind`, and
  `isRecurring` distinguishes recurring (`cron`/`rate`) expressions from
  `at(...)` one-time schedules.
- Timezone-aware helpers: `AWS.EventBridge.Schedule` pairs expressions with an
  IANA timezone (via the `tz`/`tzdata` packages) and exposes `nextRunTimes`
  variants for UTC, local, or fully-zoned outputs.
- Convenience constructors accept canonical IANA names (`"America/New_York"`)
  in addition to the generated `TZLabel` constructors.
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

-- Introspect the parsed expression without re-parsing downstream.
kindExample :: Either String ScheduleKind
kindExample = scheduleKind <$> parseCronText "rate(5 minutes)"
-- Right RateSchedule

isRecurringExample :: Either String Bool
isRecurringExample = isRecurring <$> parseCronText "at(2025-11-16T09:30:00)"
-- Right False
```

### Timezone-Aware Schedules

EventBridge rules can set a schedule timezone. Use `AWS.EventBridge.Schedule` to
bind an expression to a `TZLabel` so you can request run times in UTC, as local
wall-clock values, or as `ZonedTime`s tagged with the appropriate offset.

```haskell
import AWS.EventBridge.Schedule
import Data.Time (UTCTime(..), LocalTime, ZonedTime, fromGregorian)
import Data.Time.Zones.All (TZLabel(..))

baseUTC :: UTCTime
baseUTC = UTCTime (fromGregorian 2025 11 1) 0

zonedSchedule :: Either String Schedule
zonedSchedule = scheduleFromText America__New_York "cron(0 9 * NOV ? 2025)"

utcRuns :: Either String [UTCTime]
utcRuns = nextRunTimesUTC <$> zonedSchedule <*> pure baseUTC <*> pure 2
-- Right [2025-11-01 13:00:00 UTC,2025-11-02 14:00:00 UTC]

localRuns :: Either String [LocalTime]
localRuns = nextRunTimesLocalFromUTC <$> zonedSchedule <*> pure baseUTC <*> pure 2
-- Right [2025-11-01 09:00:00,2025-11-02 09:00:00]

zonedRuns :: Either String [ZonedTime]
zonedRuns = nextRunTimesZonedFromUTC <$> zonedSchedule <*> pure baseUTC <*> pure 2
-- Right [2025-11-01 09:00:00 EDT,2025-11-02 09:00:00 EST]
```

Every combination of base input (`UTCTime`, `LocalTime`, `ZonedTime`) and output
form is available, so you can normalize timestamps at the edges of your system
and avoid comparing values that silently belong to different timezones.

### API Overview

1. Parse using `parseCronText` (UTC) or `scheduleFromText` (timezone-aware).
2. Wrap with `scheduleFromExpr`/`scheduleFromText` if you need timezone metadata.
3. Choose an evaluation helper based on the base input you have and the output
  you need. Prefer the primary trio (`nextRunTimesUTC`, `nextRunTimesLocal`,
  `nextRunTimesZoned`) and reach for the conversion helpers when you want to
  avoid manual conversions.

<table>
  <thead>
    <tr>
      <th>Base input</th>
      <th>Output</th>
      <th>Function</th>
    </tr>
  </thead>
  <tbody>
    <tr><td><code>UTCTime</code></td><td><code>UTCTime</code></td><td><code>nextRunTimesUTC</code></td></tr>
    <tr><td><code>LocalTime</code></td><td><code>UTCTime</code></td><td><code>nextRunTimesUTCFromLocal</code></td></tr>
    <tr><td><code>ZonedTime</code></td><td><code>UTCTime</code></td><td><code>nextRunTimesUTCFromZoned</code></td></tr>
    <tr><td><code>UTCTime</code></td><td><code>LocalTime</code></td><td><code>nextRunTimesLocalFromUTC</code></td></tr>
    <tr><td><code>LocalTime</code></td><td><code>LocalTime</code></td><td><code>nextRunTimesLocal</code></td></tr>
    <tr><td><code>ZonedTime</code></td><td><code>LocalTime</code></td><td><code>nextRunTimesLocalFromZoned</code></td></tr>
    <tr><td><code>UTCTime</code></td><td><code>ZonedTime</code></td><td><code>nextRunTimesZonedFromUTC</code></td></tr>
    <tr><td><code>LocalTime</code></td><td><code>ZonedTime</code></td><td><code>nextRunTimesZonedFromLocal</code></td></tr>
    <tr><td><code>ZonedTime</code></td><td><code>ZonedTime</code></td><td><code>nextRunTimesZoned</code></td></tr>
  </tbody>
</table>

Use the conversion helpers when you already have a base timestamp in a specific
representation and want the library to handle the translation for you (for
example, UI-provided `LocalTime` that needs to be compared against UTC events).

#### Working With IANA Names

Prefer `scheduleFromText`/`scheduleFromExpr` when compiled code can depend on
`TZLabel` constructors (they offer total coverage at compile time). When
configurations or API payloads give you canonical timezone names, switch to the
IANA-aware wrappers:

- `scheduleFromExprIANA :: Text -> CronExprT -> Either String Schedule`
- `scheduleFromTextIANA :: Text -> Text -> Either String Schedule`
- `parseCronTextWithIANA :: Text -> Text -> Either String Schedule`

These helpers validate the provided timezone name against the bundled tz
database and return `Left` if it is unknown, preventing silent fallbacks.

<details>
  <summary>Example: parse config payloads with canonical names</summary>

```haskell
import AWS.EventBridge.Schedule
import Data.Text (Text)
import Data.Time (LocalTime)

mkScheduleFromConfig :: Text -> Text -> Either String Schedule
mkScheduleFromConfig tzName exprText =
  scheduleFromTextIANA tzName exprText

example :: Either String [LocalTime]
example = do
  sched <- mkScheduleFromConfig "Asia/Kolkata" "cron(0 9 * * ? *)"
  nextRunTimesLocal sched (read "2025-11-15 09:00:00" :: LocalTime) 2
```

</details>

<details>
  <summary>Example: fall back to a default timezone</summary>

```haskell
import AWS.EventBridge.Schedule
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Time (UTCTime)

resolveSchedule :: Maybe Text -> Text -> Either String Schedule
resolveSchedule maybeName exprText = do
  let tzName = fromMaybe "UTC" maybeName
  scheduleFromTextIANA tzName exprText

fromApi :: Either String [UTCTime]
fromApi = do
  sched <- resolveSchedule (Just "America/Los_Angeles") "cron(0 9 * * ? *)"
  nextRunTimesUTC sched (read "2025-11-01 17:00:00 UTC" :: UTCTime) 1
```

</details>

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
cabal bench
cabal haddock --open
```

`cabal bench` executes a Criterion suite that profiles cron-heavy workloads,
rate schedules, and timezone-aware helpers so you can gauge regression risk
when modifying the evaluator.

## Contributing

Bug reports, suggestions, and pull requests are welcome. Please open an issue
before large-scale changes so we can keep the API coherent.

## License

Released under the BSD-3-Clause license. See `LICENSE` for details.

## References

- [AWS EventBridge cron expression documentation][aws-docs]

[aws-docs]: https://docs.aws.amazon.com/scheduler/latest/UserGuide/schedule-types.html#cron-based