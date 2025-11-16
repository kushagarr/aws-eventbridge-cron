{-# LANGUAGE OverloadedStrings #-}

module AWS.EventBridge.CronSpec (tests) where

import AWS.EventBridge.Cron
import AWS.EventBridge.Minutes (parseMinutesText)
import AWS.EventBridge.Hours (parseHoursText)
import AWS.EventBridge.DayOfMonth (parseDayOfMonthText)
import AWS.EventBridge.Months (parseMonthsText)
import AWS.EventBridge.DayOfWeek (parseDayOfWeekText)
import AWS.EventBridge.Years (parseYearsText)
import Data.Time (UTCTime (..), addUTCTime, secondsToNominalDiffTime)
import Data.Time.Calendar (fromGregorian)
import Data.Time.LocalTime (TimeOfDay (..), timeOfDayToTime)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import Test.Tasty.QuickCheck as QC
import TestSupport (assertLeft, expectParseWith)
import qualified Data.Text as T

tests :: TestTree
tests =
  testGroup "cron"
    [ manualTests
    , propertyTests
    ]

manualTests :: TestTree
manualTests = testGroup "manual"
  [ testCase "parse cron expression" $ do
      cronExpr <- expectParseWith "cron" parseCronText "cron(0 0 ? * 2#1 2025)"
      expectedMinutes <- expectParseWith "minutes" parseMinutesText "0"
      expectedHours <- expectParseWith "hours" parseHoursText "0"
      expectedDom <- expectParseWith "dom" parseDayOfMonthText "?"
      expectedMonths <- expectParseWith "months" parseMonthsText "*"
      expectedDow <- expectParseWith "dow" parseDayOfWeekText "2#1"
      expectedYears <- expectParseWith "years" parseYearsText "2025"
      case cronExpr of
        CronExpr mins hrs dom months dow years -> do
          mins @?= expectedMinutes
          hrs @?= expectedHours
          dom @?= expectedDom
          months @?= expectedMonths
          dow @?= expectedDow
          years @?= expectedYears
        _ -> assertFailure "expected CronExpr variant"
  , testCase "next run times rate" $ do
      let base = UTCTime (fromGregorian 2025 11 16) (timeOfDayToTime (TimeOfDay 9 0 0))
      rateExpr <- expectParseWith "rate" parseCronText "rate(5 minutes)"
      nextRunTimes rateExpr base 3
        @?= Right
          [ base
          , addMinutes base 5
          , addMinutes base 10
          ]
  , testCase "next run times one-time" $ do
      let base = UTCTime (fromGregorian 2025 11 16) (timeOfDayToTime (TimeOfDay 9 0 0))
      oneTimeExpr <- expectParseWith "one-time" parseCronText "at(2025-11-16T09:30:00)"
      nextRunTimes oneTimeExpr base 1
        @?= Right [UTCTime (fromGregorian 2025 11 16) (timeOfDayToTime (TimeOfDay 9 30 0))]
  , testCase "next run times cron" $ do
      let base = UTCTime (fromGregorian 2025 11 16) (timeOfDayToTime (TimeOfDay 9 0 0))
      cronExpr <- expectParseWith "cron" parseCronText "cron(0/15 9 ? NOV SUN 2025)"
      nextRunTimes cronExpr base 4
        @?= Right
          [ UTCTime (fromGregorian 2025 11 16) (timeOfDayToTime (TimeOfDay 9 0 0))
          , UTCTime (fromGregorian 2025 11 16) (timeOfDayToTime (TimeOfDay 9 15 0))
          , UTCTime (fromGregorian 2025 11 16) (timeOfDayToTime (TimeOfDay 9 30 0))
          , UTCTime (fromGregorian 2025 11 16) (timeOfDayToTime (TimeOfDay 9 45 0))
          ]
  , testCase "invalid day fields combination" $ do
      cronExpr <- expectParseWith "cron" parseCronText "cron(0 0 1 * 2 2025)"
      nextRunTimes cronExpr (UTCTime (fromGregorian 2025 1 1) 0) 1
        @?= Left "day-of-month and day-of-week fields must use '?' in exactly one position"
  , testCase "invalid rate" $ do
      rateExpr <- expectParseWith "rate" parseCronText "rate(0 minutes)"
      let base = UTCTime (fromGregorian 2025 1 1) 0
      nextRunTimes rateExpr base 1
        @?= Left "invalid rate minutes: 0 (expected 1..31536000)"
  , testCase "invalid one-time" $
      assertLeft (parseCronText "at(2025-13-01T00:00:00)")
  ]

propertyTests :: TestTree
propertyTests = testGroup "properties"
  [ QC.testProperty "rate schedule monotonic" propRateMonotonic
  , QC.testProperty "cron returns at most requested" propCronLimit
  ]

propRateMonotonic :: QC.Property
propRateMonotonic =
  QC.forAll (QC.chooseInt (1, 60)) $ \minutes ->
    let base = UTCTime (fromGregorian 2025 1 1) (timeOfDayToTime (TimeOfDay 0 0 0))
     in case parseCronText ("rate(" <> T.pack (show minutes) <> " minutes)") of
          Left err -> QC.counterexample ("rate parse failed: " <> err) False
          Right rateExpr ->
            case nextRunTimes rateExpr base 5 of
              Left err -> QC.counterexample ("schedule failed: " <> err) False
              Right ts -> QC.counterexample (show ts) (sorted ts)
  where
    sorted xs = and (zipWith (<=) xs (drop 1 xs))

propCronLimit :: QC.Property
propCronLimit =
  QC.forAll (QC.chooseInt (1, 5)) $ \limit ->
    let base = UTCTime (fromGregorian 2025 11 16) (timeOfDayToTime (TimeOfDay 9 0 0))
     in case parseCronText "cron(0/30 9 ? NOV SUN 2025)" of
          Left err -> QC.counterexample ("cron parse failed: " <> err) False
          Right cronExpr ->
            case nextRunTimes cronExpr base limit of
              Left err -> QC.counterexample ("cron evaluation failed: " <> err) False
              Right ts -> QC.counterexample (show ts) (length ts <= limit)

addMinutes :: UTCTime -> Integer -> UTCTime
addMinutes t minutes = addUTCTime (secondsToNominalDiffTime (fromIntegral (minutes * 60))) t