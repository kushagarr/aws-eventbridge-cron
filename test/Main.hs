module Main (main) where

import qualified AWS.EventBridge.DayOfMonthSpec as DayOfMonthSpec
import qualified AWS.EventBridge.DayOfWeekSpec as DayOfWeekSpec
import qualified AWS.EventBridge.HoursSpec as HoursSpec
import qualified AWS.EventBridge.MinutesSpec as MinutesSpec
import qualified AWS.EventBridge.MonthsSpec as MonthsSpec
import qualified AWS.EventBridge.YearsSpec as YearsSpec
import qualified AWS.EventBridge.RateSpec as RateSpec
import qualified AWS.EventBridge.OneTimeSpec as OneTimeSpec
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main =
  defaultMain $
    testGroup "aws-eventbridge-cron"
      [ DayOfMonthSpec.tests
      , DayOfWeekSpec.tests
      , HoursSpec.tests
      , MinutesSpec.tests
      , MonthsSpec.tests
      , YearsSpec.tests
      , RateSpec.tests
      , OneTimeSpec.tests
      ]
