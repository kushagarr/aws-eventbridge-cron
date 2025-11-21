{-# LANGUAGE RecordWildCards #-}

-- | Zone-aware helpers built on top of 'AWS.EventBridge.Cron'.
--
-- == Choosing an entry point
--
-- * Parse the expression: use 'parseCronText' (UTC-only) or 'scheduleFromText'
--   when the rule already specifies an IANA timezone.
-- * Wrap the expression with 'scheduleFromExpr' or 'scheduleFromText' to carry
--   timezone metadata.
-- * Evaluate upcoming runs via the primary trio:
--
--   * 'nextRunTimesUTC'   – keep everything in UTC and compare against other
--     absolute timestamps.
--   * 'nextRunTimesLocal' – receive wall-clock values in the schedule's zone.
--   * 'nextRunTimesZoned' – like 'nextRunTimesLocal' but tagged with the
--     'TimeZone' used at each occurrence (captures DST changes).
--
-- Conversion helpers (the @*FromUTC@, @*FromLocal@, and @*FromZoned@ variants)
-- simply preprocess the base value before delegating to one of the three
-- primary functions. They are handy when your caller already has a specific
-- representation and you want to avoid manual conversions.
--
-- == Base/Input vs Output quick reference
--
-- @
-- Base input  Output      Function
-- ----------------------------------------------
-- UTCTime     UTCTime     nextRunTimesUTC
-- LocalTime   UTCTime     nextRunTimesUTCFromLocal
-- ZonedTime   UTCTime     nextRunTimesUTCFromZoned
-- UTCTime     LocalTime   nextRunTimesLocalFromUTC
-- LocalTime   LocalTime   nextRunTimesLocal
-- ZonedTime   LocalTime   nextRunTimesLocalFromZoned
-- UTCTime     ZonedTime   nextRunTimesZonedFromUTC
-- LocalTime   ZonedTime   nextRunTimesZonedFromLocal
-- ZonedTime   ZonedTime   nextRunTimesZoned
-- @
module AWS.EventBridge.Schedule
  ( -- * Schedule construction
    Schedule(..)
  , scheduleFromExpr
  , scheduleFromExprIANA
  , scheduleFromText
  , scheduleFromTextIANA
  , parseCronTextWithZone
  , parseCronTextWithIANA
    -- * Primary evaluation helpers
  , nextRunTimesUTC
  , nextRunTimesLocal
  , nextRunTimesZoned
    -- * Conversion helpers
  , nextRunTimesUTCFromLocal
  , nextRunTimesUTCFromZoned
  , nextRunTimesLocalFromUTC
  , nextRunTimesLocalFromZoned
  , nextRunTimesZonedFromUTC
  , nextRunTimesZonedFromLocal
  ) where

import AWS.EventBridge.Cron
  ( CronExprT
  , ScheduleKind(..)
  , nextRunTimes
  , parseCronText
  , scheduleKind
  )
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Time
  ( LocalTime(..)
  , ZonedTime(..)
  , UTCTime(..)
  , timeOfDayToTime
  , timeToTimeOfDay
  , zonedTimeToUTC
  )
import Data.Time.Zones
  ( TZ
  , localTimeToUTCTZ
  , timeZoneForUTCTime
  , utcToLocalTimeTZ
  )
import Data.Time.Zones.All (TZLabel(..), fromTZName, tzByLabel)

-- | Scheduling expression paired with its IANA timezone.
--
-- Use 'scheduleFromText' when you need to parse the expression and bind the
-- timezone in one step, or 'scheduleFromExpr' if you already have a
-- 'CronExprT'.
data Schedule = Schedule
  { scheduleExpr :: CronExprT
  , scheduleZone :: TZ
  , scheduleZoneLabel :: TZLabel
  }

-- | Construct a 'Schedule' from an existing 'CronExprT' and timezone label.
--
-- @tz@ ships with bindings for the entire IANA database through 'TZLabel'. The
-- resulting schedule can be fed directly into the zone-aware 'nextRunTimes*'
-- helpers below.
scheduleFromExpr :: TZLabel -> CronExprT -> Schedule
scheduleFromExpr label expr = Schedule
  { scheduleExpr = expr
  , scheduleZone = tzByLabel label
  , scheduleZoneLabel = label
  }

-- | Construct a 'Schedule' directly from an IANA location name such as
-- @"America/New_York"@. This is a convenience wrapper over
-- 'scheduleFromExpr' for callers that already store timezone identifiers as
-- strings. Returns 'Left' when the name is unknown to the bundled tz database.
scheduleFromExprIANA :: Text -> CronExprT -> Either String Schedule
scheduleFromExprIANA tzName expr =
  scheduleFromExpr <$> resolveTZLabel tzName <*> pure expr

-- | Parse an EventBridge expression and attach a timezone in the same step.
--
-- >>> :{
-- let base = read "2025-11-16 03:30:00 UTC" :: UTCTime
-- in case scheduleFromText Asia__Kolkata "cron(0 9 ? NOV SUN 2025)" of
--      Left err -> Left err
--      Right sched -> nextRunTimesUTC sched base 1
-- :}
-- Right [2025-11-16 03:30:00 UTC]
scheduleFromText :: TZLabel -> Text -> Either String Schedule
scheduleFromText label input =
  fmap (scheduleFromExpr label) (parseCronText input)

-- | Parse an EventBridge expression and attach a timezone via its IANA name
-- (for example @"Asia/Kolkata"@). This helper surfaces nicer ergonomics for
-- API payloads or configuration files that keep the canonical string form.
--
-- >>> let base = read "2025-11-16 03:30:00 UTC" :: UTCTime
-- >>> scheduleFromTextIANA "Asia/Kolkata" "cron(0 9 ? NOV SUN 2025)" >>= \sched -> nextRunTimesUTC sched base 1
-- Right [2025-11-16 03:30:00 UTC]
scheduleFromTextIANA :: Text -> Text -> Either String Schedule
scheduleFromTextIANA tzName input =
  resolveTZLabel tzName >>= \label -> scheduleFromText label input

-- | Backwards-compatible alias for 'scheduleFromText'.
parseCronTextWithZone :: TZLabel -> Text -> Either String Schedule
parseCronTextWithZone = scheduleFromText

-- | Alias for 'scheduleFromTextIANA'.
--
-- >>> parseCronTextWithIANA "America/New_York" "cron(0 9 * * ? *)" >>= \sched -> nextRunTimesLocal sched (read "2025-11-01 08:30:00" :: LocalTime) 1
-- Right [2025-11-01 09:00:00]
parseCronTextWithIANA :: Text -> Text -> Either String Schedule
parseCronTextWithIANA = scheduleFromTextIANA

-- | Local-time primary helper.
--
-- Evaluate run times in the schedule's local timezone, starting from a local
-- base time that is already expressed in the schedule's zone. Choose this when
-- you want to present results exactly as the rule owner configured them.
--
-- >>> let Right sched = scheduleFromText America__New_York "cron(0 9 * * ? *)"
-- >>> let base = read "2025-11-01 08:00:00" :: LocalTime
-- >>> nextRunTimesLocal sched base 1
-- Right [2025-11-01 09:00:00]
nextRunTimesLocal :: Schedule -> LocalTime -> Int -> Either String [LocalTime]
nextRunTimesLocal = scheduleLocalOccurrences

-- | Conversion helper.
--
-- Evaluate run times in the schedule's local timezone using a UTC base. Ideal
-- when upstream systems give you absolute timestamps (e.g. database clocks) but
-- UI clients expect the local wall clock.
--
-- >>> let Right sched = scheduleFromText America__New_York "cron(0 9 * * ? *)"
-- >>> let base = read "2025-11-01 12:00:00 UTC" :: UTCTime
-- >>> nextRunTimesLocalFromUTC sched base 1
-- Right [2025-11-01 09:00:00]
nextRunTimesLocalFromUTC :: Schedule -> UTCTime -> Int -> Either String [LocalTime]
nextRunTimesLocalFromUTC schedule base = scheduleLocalOccurrences schedule (utcToLocalTimeTZ (scheduleZone schedule) base)

-- | Conversion helper.
--
-- Evaluate run times in the schedule's local timezone using an arbitrary
-- 'ZonedTime' base (the offset on the input is ignored; only the instant
-- matters). Handy when you get user input such as "2025-11-01 09:00 EDT" and
-- want to keep working in local values.
--
-- >>> let Right sched = scheduleFromText America__New_York "cron(0 9 * * ? *)"
-- >>> let base = read "2025-11-01 09:00:00-04:00" :: ZonedTime
-- >>> nextRunTimesLocalFromZoned sched base 2
-- Right [2025-11-01 09:00:00,2025-11-02 09:00:00]
nextRunTimesLocalFromZoned :: Schedule -> ZonedTime -> Int -> Either String [LocalTime]
nextRunTimesLocalFromZoned schedule base = scheduleLocalOccurrences schedule (utcToLocalTimeTZ (scheduleZone schedule) (zonedTimeToUTC base))

-- | UTC primary helper.
--
-- Evaluate run times in UTC while accepting a UTC base. This is the simplest
-- option when the rest of your system already speaks UTC.
--
-- >>> let Right sched = scheduleFromText America__New_York "cron(0 9 * * ? *)"
-- >>> let base = read "2025-11-01 12:30:00 UTC" :: UTCTime
-- >>> nextRunTimesUTC sched base 1
-- Right [2025-11-01 13:00:00 UTC]
nextRunTimesUTC :: Schedule -> UTCTime -> Int -> Either String [UTCTime]
nextRunTimesUTC schedule base limit =
  fmap (map (localToUTC schedule)) (nextRunTimesLocalFromUTC schedule base limit)

-- | Conversion helper.
--
-- Evaluate run times in UTC with a local base. Use this when your caller has
-- local wall time but downstream systems expect UTC instants.
--
-- >>> let Right sched = scheduleFromText America__New_York "cron(0 9 * * ? *)"
-- >>> let base = read "2025-11-01 08:00:00" :: LocalTime
-- >>> nextRunTimesUTCFromLocal sched base 1
-- Right [2025-11-01 13:00:00 UTC]
nextRunTimesUTCFromLocal :: Schedule -> LocalTime -> Int -> Either String [UTCTime]
nextRunTimesUTCFromLocal schedule base limit =
  fmap (map (localToUTC schedule)) (nextRunTimesLocal schedule base limit)

-- | Conversion helper.
--
-- Evaluate run times in UTC with a 'ZonedTime' base. Helpful when upstream APIs
-- hand you zoned timestamps and you want to compare the schedule against other
-- UTC data.
--
-- >>> let Right sched = scheduleFromText America__New_York "cron(0 9 * * ? *)"
-- >>> let base = read "2025-11-01 09:00:00-04:00" :: ZonedTime
-- >>> nextRunTimesUTCFromZoned sched base 1
-- Right [2025-11-01 13:00:00 UTC]
nextRunTimesUTCFromZoned :: Schedule -> ZonedTime -> Int -> Either String [UTCTime]
nextRunTimesUTCFromZoned schedule base limit =
  fmap (map (localToUTC schedule)) (nextRunTimesLocalFromZoned schedule base limit)

-- | Zoned-time primary helper.
--
-- Evaluate run times as 'ZonedTime' values using a zoned base. This preserves
-- the original offset used for the base and propagates DST changes into the
-- outputs, which is useful for logs or API responses that must mention the
-- effective offset explicitly.
--
-- >>> let Right sched = scheduleFromText America__New_York "cron(0 9 * * ? *)"
-- >>> let base = read "2025-11-01 09:00:00-04:00" :: ZonedTime
-- >>> nextRunTimesZoned sched base 2
-- Right [2025-11-01 09:00:00-04:00,2025-11-02 09:00:00-05:00]
nextRunTimesZoned :: Schedule -> ZonedTime -> Int -> Either String [ZonedTime]
nextRunTimesZoned schedule base limit =
  fmap (map (localToZoned schedule)) (nextRunTimesLocalFromZoned schedule base limit)

-- | Conversion helper.
--
-- Evaluate run times as 'ZonedTime' values from a UTC base.
--
-- >>> let Right sched = scheduleFromText America__New_York "cron(0 9 * * ? *)"
-- >>> let base = read "2025-11-01 12:30:00 UTC" :: UTCTime
-- >>> nextRunTimesZonedFromUTC sched base 2
-- Right [2025-11-01 09:00:00-04:00,2025-11-02 09:00:00-05:00]
nextRunTimesZonedFromUTC :: Schedule -> UTCTime -> Int -> Either String [ZonedTime]
nextRunTimesZonedFromUTC schedule base limit =
  fmap (map (localToZoned schedule)) (nextRunTimesLocalFromUTC schedule base limit)

-- | Conversion helper.
--
-- Evaluate run times as 'ZonedTime' values from a local base.
--
-- >>> let Right sched = scheduleFromText America__New_York "cron(0 9 * * ? *)"
-- >>> let base = read "2025-11-01 09:00:00" :: LocalTime
-- >>> nextRunTimesZonedFromLocal sched base 1
-- Right [2025-11-01 09:00:00-04:00]
nextRunTimesZonedFromLocal :: Schedule -> LocalTime -> Int -> Either String [ZonedTime]
nextRunTimesZonedFromLocal schedule base limit =
  fmap (map (localToZoned schedule)) (nextRunTimesLocal schedule base limit)

scheduleLocalOccurrences :: Schedule -> LocalTime -> Int -> Either String [LocalTime]
scheduleLocalOccurrences sched@Schedule{..} base limit =
  case scheduleKind scheduleExpr of
    RateSchedule -> do
      let baseUtc = localToUTC sched base
      timesUtc <- nextRunTimes scheduleExpr baseUtc limit
      pure (map (utcToLocalTimeTZ scheduleZone) timesUtc)
    _ ->
      fmap (map utcToLocalNaive) (nextRunTimes scheduleExpr (localToNaiveUTC base) limit)

localToNaiveUTC :: LocalTime -> UTCTime
localToNaiveUTC (LocalTime day tod) = UTCTime day (timeOfDayToTime tod)

utcToLocalNaive :: UTCTime -> LocalTime
utcToLocalNaive (UTCTime day diff) = LocalTime day (timeToTimeOfDay diff)

localToUTC :: Schedule -> LocalTime -> UTCTime
localToUTC Schedule{..} = localTimeToUTCTZ scheduleZone

localToZoned :: Schedule -> LocalTime -> ZonedTime
localToZoned schedule local =
  let utcVal = localToUTC schedule local
      zone = scheduleZone schedule
      localWall = utcToLocalTimeTZ zone utcVal
      tzInfo = timeZoneForUTCTime zone utcVal
   in ZonedTime localWall tzInfo

resolveTZLabel :: Text -> Either String TZLabel
resolveTZLabel tzName =
  maybe (Left errMsg) Right (fromTZName (encodeUtf8 tzName))
  where
    errMsg = "unknown IANA timezone: " <> T.unpack tzName