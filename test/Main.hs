module Main (main) where

import qualified AWS.EventBridge.DayOfMonthSpec as DayOfMonthSpec
import qualified AWS.EventBridge.HoursSpec as HoursSpec
import qualified AWS.EventBridge.MinutesSpec as MinutesSpec
import qualified AWS.EventBridge.MonthsSpec as MonthsSpec
import qualified AWS.EventBridge.YearsSpec as YearsSpec
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main =
  defaultMain $
    testGroup "aws-eventbridge-cron"
      [ DayOfMonthSpec.tests
      , HoursSpec.tests
      , MinutesSpec.tests
      , MonthsSpec.tests
      , YearsSpec.tests
      ]
