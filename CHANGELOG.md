# Changelog

All notable changes to this project will be documented in this file.

The format is based on Keep a Changelog and this project adheres to
Semantic Versioning.

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