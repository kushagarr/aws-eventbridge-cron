{-# LANGUAGE OverloadedStrings #-}

module AWS.EventBridge.OneTimeSpec (tests) where

import AWS.EventBridge.OneTime
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime (..))
import Data.Time.Calendar (fromGregorian, gregorianMonthLength)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Time.LocalTime (TimeOfDay (..), timeOfDayToTime)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck as QC
import TestSupport (assertLeft)

tests :: TestTree
tests =
  testGroup "one-time"
    [ manualTests
    , propertyTests
    ]

manualTests :: TestTree
manualTests = testGroup "manual"
  [ testCase "parse at expression" $
      parseOneTimeText "at(2025-11-16T09:30:00)"
        @?= Right (OneTime (mkUtc 2025 11 16 9 30 0))
  , testCase "parse allows whitespace" $
      parseOneTimeText "  at(2025-01-01T00:00:00)  "
        @?= Right (OneTime (mkUtc 2025 1 1 0 0 0))
  , testCase "parse rejects Z suffix" $
      assertLeft (parseOneTimeText "at(2025-03-15T12:45:30Z)")
  , testCase "evaluator returns timestamp" $
      evaluateOneTimeT (OneTime (mkUtc 2024 5 20 18 0 0))
        @?= mkUtc 2024 5 20 18 0 0
  , testCase "invalid month rejected" $
      assertLeft (parseOneTimeText "at(2025-13-01T00:00:00)")
  , testCase "invalid day rejected" $
      assertLeft (parseOneTimeText "at(2025-02-30T00:00:00)")
  , testCase "invalid time rejected" $
      assertLeft (parseOneTimeText "at(2025-01-01T25:00:00)")
  , testCase "missing seconds rejected" $
      assertLeft (parseOneTimeText "at(2025-01-01T00:00)")
  ]

propertyTests :: TestTree
propertyTests = testGroup "properties"
  [ QC.testProperty "parser round-trips formatted timestamps" propRoundTrip
  ]

propRoundTrip :: QC.Property
propRoundTrip =
  QC.forAll genUtc $ \t ->
    let rendered = renderAt t
     in parseOneTimeText rendered QC.=== Right (OneTime t)

renderAt :: UTCTime -> Text
renderAt t =
  let payload = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S" t
   in T.pack ("at(" <> payload <> ")")

mkUtc :: Integer -> Int -> Int -> Int -> Int -> Int -> UTCTime
mkUtc year month day hour minute second =
  let day' = fromGregorian year month day
      tod = TimeOfDay hour minute (fromIntegral second)
   in UTCTime day' (timeOfDayToTime tod)

genUtc :: QC.Gen UTCTime
genUtc = do
  year <- QC.chooseInteger (1970, 2199)
  month <- QC.chooseInt (1, 12)
  let dim = gregorianMonthLength year month
  day <- QC.chooseInt (1, dim)
  hour <- QC.chooseInt (0, 23)
  minute <- QC.chooseInt (0, 59)
  second <- QC.chooseInt (0, 59)
  pure (mkUtc year month day hour minute second)