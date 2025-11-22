# Changelog

All notable changes to this project will be documented in this file.

The format is based on Keep a Changelog and this project adheres to
Semantic Versioning.

## [0.2.0.0] - 2025-11-22
### Changed
- Reordered `nextRunTimes` and all schedule helper signatures to accept `(base -> limit -> schedule/expr)` so callers can compose with `>>=`/`fmap` without lambdas. This is a breaking change for all call sites, including the exported zone-aware helpers and benchmark functions.
### Fixed
- README, Haddocks, tests, and benchmarks now show the new argument order to prevent confusion when following examples.

## [0.1.2.1] - 2025-11-22
### Added
- `Show` instance for `Schedule` to facilitate REPL debugging (displays the expression and timezone label).

## [0.1.2.0] - 2025-11-21
### Added
- Convenience constructors (`scheduleFromExprIANA`, `scheduleFromTextIANA`, and
	`parseCronTextWithIANA`) so callers can bind schedules using canonical IANA
	timezone names without touching `TZLabel`.
- Criterion benchmark suite (`cabal bench`) that exercises sparse/dense cron
	workloads, rate schedules, and timezone-aware helpers to catch performance
	regressions.

### Changed
- Optimised `futureCronTimes` to use difference lists and lazily compute only
	the necessary day candidates, removing quadratic `++` chains and improving
	large run queries by ~30%.
- README now uses an HTML function matrix that renders correctly on Hackage,
	adds foldable IANA examples, and mentions the benchmark workflow.
- Enabled `tests: True` in `cabal.project` so editors and `cabal test` pick up
	the suite without extra flags.

## [0.1.1.1] - 2025-11-20
### Changed
- Improved Haddock/README guidance for timezone-aware helpers, including a
	rendered-friendly entrypoint walkthrough and base-vs-output reference table.

## [0.1.1.0] - 2025-11-20
### Added
- `AWS.EventBridge.Schedule` module that pairs schedules with an IANA timezone
	(via the `tz`/`tzdata` packages) and exposes the full matrix of
	`nextRunTimes` variants for UTC, local, and zoned outputs.
- Smart constructors such as `scheduleFromText` plus helper variants
	(`nextRunTimesUTCFromLocal`, `nextRunTimesZonedFromUTC`, etc.) documented with
	Haddock examples.
- Property-style regression tests in `ScheduleSpec` covering cron/one-time rate
	conversions, DST transitions, and timezone-dependent outputs.
- README examples describing timezone-aware workflows and feature list updates.

## [0.1.0.1] - 2025-11-17
### Added
- `ScheduleKind`, `scheduleKind`, and `isRecurring` helpers for classifying parsed schedules without re-parsing downstream.
- QuickCheck properties covering the new helpers to ensure they align with parsed expression variants.
- README documentation demonstrating schedule introspection and clarifying the helper API.

## [0.1.0.0] - 2025-11-16
### Added
- Initial `AWS.EventBridge.Cron` module for parsing and scheduling AWS Scheduler cron expressions.
- Support for `rate(...)` and `at(...)` expressions with validation, evaluation, and round-trip tests.
- Property-based and unit tests across all cron fields plus helper utilities.