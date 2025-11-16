# Changelog

All notable changes to this project will be documented in this file.

The format is based on Keep a Changelog and this project adheres to
Semantic Versioning.

## [0.1.0.0] - 2025-11-16
### Added
- Initial `AWS.EventBridge.Cron` module for parsing and scheduling AWS Scheduler cron expressions.
- Support for `rate(...)` and `at(...)` expressions with validation, evaluation, and round-trip tests.
- Property-based and unit tests across all cron fields plus helper utilities.
- Project metadata, README, LICENSE, and Haddock documentation suitable for Hackage distribution.
- GitHub Actions workflows for CI and release automation, including Hackage candidate/publish steps.