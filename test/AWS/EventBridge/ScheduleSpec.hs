{-# LANGUAGE OverloadedStrings #-}

module AWS.EventBridge.ScheduleSpec (tests) where

import AWS.EventBridge.Schedule
import AWS.EventBridge.Cron (parseCronText)
import Data.Time
  ( LocalTime
  , UTCTime(..)
  , ZonedTime
  , addUTCTime
  , fromGregorian
  , secondsToDiffTime
  )
import Data.Time.Zones.All (TZLabel(..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "schedule"
  [ testCase "cron nextRunTimesUTC respects timezone" $ do
      sched <- either assertFailure pure (scheduleFromText Asia__Kolkata "cron(0 9 * * ? *)")
      let base = UTCTime (fromGregorian 2025 11 16) 0
          expected = UTCTime (fromGregorian 2025 11 16) (secondsToDiffTime (3 * 3600 + 30 * 60))
      nextRunTimesUTC sched base 1 @?= Right [expected]

    , testCase "cron zoned outputs reflect DST" $ do
      sched <- either assertFailure pure (scheduleFromText America__New_York "cron(0 9 * * ? *)")
      let base = read "2025-11-01 09:00:00-04:00" :: ZonedTime
          expected =
            [ "2025-11-01 09:00:00 EDT"
            , "2025-11-02 09:00:00 EST"
            ]

      fmap (map show) (nextRunTimesZoned sched base 2) @?= Right expected

  , testCase "rate expressions remain absolute" $ do
      expr <- either assertFailure pure (parseCronText "rate(1 hour)")
      let sched = scheduleFromExpr Asia__Tokyo expr
          base = UTCTime (fromGregorian 2025 11 1) 0
          expected = take 2 (iterate (addUTCTime 3600) base)
      nextRunTimesUTC sched base 2 @?= Right expected

    , testCase "local helper trims times before base" $ do
      sched <- either assertFailure pure (scheduleFromText Asia__Kolkata "cron(0 9 * * ? *)")
      let baseLocal = read "2025-11-16 08:30:00" :: LocalTime
          expected =
            [ read "2025-11-16 09:00:00" :: LocalTime
            , read "2025-11-17 09:00:00"
            ]
      nextRunTimesLocal sched baseLocal 2 @?= Right expected

    , testCase "local-from-UTC matches local helper" $ do
      sched <- either assertFailure pure (scheduleFromText Asia__Kolkata "cron(0 9 * * ? *)")
      let baseUtc = UTCTime (fromGregorian 2025 11 16) (secondsToDiffTime (3 * 3600 + 30 * 60))
      nextRunTimesLocalFromUTC sched baseUtc 1
        @?= nextRunTimesLocal sched (read "2025-11-16 09:00:00" :: LocalTime) 1

    , testCase "UTC-from-local converts to absolute" $ do
      sched <- either assertFailure pure (scheduleFromText Asia__Kolkata "cron(0 9 * * ? *)")
      let baseLocal = read "2025-11-16 09:00:00" :: LocalTime
          expected = UTCTime (fromGregorian 2025 11 16) (secondsToDiffTime (3 * 3600 + 30 * 60))
      nextRunTimesUTCFromLocal sched baseLocal 1 @?= Right [expected]

    , testCase "zoned-from-UTC emits offsets" $ do
      sched <- either assertFailure pure (scheduleFromText America__New_York "cron(0 9 * * ? *)")
      let baseUtc = UTCTime (fromGregorian 2025 11 1) (secondsToDiffTime (12 * 3600))
          expected =
            [ "2025-11-01 09:00:00 EDT"
            , "2025-11-02 09:00:00 EST"
            ]
      fmap (map show) (nextRunTimesZonedFromUTC sched baseUtc 2) @?= Right expected

    , testCase "scheduleFromTextIANA resolves canonical names" $ do
        sched <- either assertFailure pure (scheduleFromTextIANA "America/New_York" "cron(0 9 * * ? *)")
        scheduleZoneLabel sched @?= America__New_York

    , testCase "scheduleFromTextIANA surfaces unknown names" $ do
        case scheduleFromTextIANA "Mars/Base" "cron(0 0 * * ? *)" of
          Left msg -> msg @?= "unknown IANA timezone: Mars/Base"
          Right _ -> assertFailure "expected a failure for an unknown timezone"

    , testCase "parseCronTextWithZone aliases scheduleFromText" $ do
      let expr = "cron(0 0 1 1 ? 2025)"
      schedA <- either assertFailure pure (parseCronTextWithZone Asia__Kolkata expr)
      schedB <- either assertFailure pure (scheduleFromText Asia__Kolkata expr)
      scheduleZoneLabel schedA @?= scheduleZoneLabel schedB
      nextRunTimesUTC schedA (UTCTime (fromGregorian 2025 1 1) 0) 1
        @?= nextRunTimesUTC schedB (UTCTime (fromGregorian 2025 1 1) 0) 1
  ]
