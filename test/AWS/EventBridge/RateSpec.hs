{-# LANGUAGE OverloadedStrings #-}

module AWS.EventBridge.RateSpec (tests) where

import AWS.EventBridge.Rate
import Data.Time.Clock (secondsToNominalDiffTime)
import qualified Data.Text as T
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck as QC
import TestSupport (assertLeft)

tests :: TestTree
tests =
  testGroup "rate"
    [ manualTests
    , propertyTests
    ]

manualTests :: TestTree
manualTests = testGroup "manual"
  [ testCase "parse minutes" $ parseRateText "rate(5 minutes)" @?= Right (RateMinutes 5)
  , testCase "parse minute singular" $ parseRateText "rate(1 minute)" @?= Right (RateMinutes 1)
  , testCase "parse hours plural" $ parseRateText "rate(10 hours)" @?= Right (RateHours 10)
  , testCase "parse hour singular" $ parseRateText "rate(1 hour)" @?= Right (RateHours 1)
  , testCase "parse days" $ parseRateText "rate(3 days)" @?= Right (RateDays 3)
  , testCase "parse day singular" $ parseRateText "rate(1 day)" @?= Right (RateDays 1)
  , testCase "parse trims surrounding whitespace" $ parseRateText "  rate(2 hours)\n" @?= Right (RateHours 2)
  , testCase "parse without separator fails" $ assertLeft (parseRateText "rate(5minutes)")
  , testCase "parse negative rejected" $ assertLeft (parseRateText "rate(-5 minutes)")
  , testCase "evaluate minutes" $
      evaluateRateT (RateMinutes 15) @?= Right (secondsToNominalDiffTime 900)
  , testCase "evaluate hours" $
      evaluateRateT (RateHours 2) @?= Right (secondsToNominalDiffTime 7200)
  , testCase "evaluate days" $
      evaluateRateT (RateDays 1) @?= Right (secondsToNominalDiffTime 86400)
  , testCase "evaluate minutes upper bound" $
    evaluateRateT (RateMinutes 31536000)
        @?= Right (secondsToNominalDiffTime (fromInteger (31536000 * 60)))
  , testCase "evaluate hours upper bound" $
    evaluateRateT (RateHours 8760)
        @?= Right (secondsToNominalDiffTime (fromInteger (8760 * 3600)))
  , testCase "evaluate days upper bound" $
    evaluateRateT (RateDays 365)
        @?= Right (secondsToNominalDiffTime (fromInteger (365 * 86400)))
  , testCase "minutes below range rejected" $
      assertLeft (evaluateRateT (RateMinutes 0))
  , testCase "minutes above range rejected" $
      assertLeft (evaluateRateT (RateMinutes 40000000))
  , testCase "hours below range rejected" $
      assertLeft (evaluateRateT (RateHours 0))
  , testCase "hours above range rejected" $
      assertLeft (evaluateRateT (RateHours 9000))
  , testCase "days below range rejected" $
      assertLeft (evaluateRateT (RateDays 0))
  , testCase "days above range rejected" $
      assertLeft (evaluateRateT (RateDays 400))
  , testCase "invalid unit fails" $
      case parseRateText "rate(5 weeks)" of
        Left _ -> pure ()
        Right val -> fail ("unexpected success: " <> show val)
  ]

propertyTests :: TestTree
propertyTests = testGroup "properties"
  [ QC.testProperty "minutes evaluate to consistent seconds" propMinutesEvaluate
  , QC.testProperty "hours evaluate to consistent seconds" propHoursEvaluate
  , QC.testProperty "days evaluate to consistent seconds" propDaysEvaluate
  , QC.testProperty "parser round-trips generated expressions" propParserRoundTrip
  ]

propMinutesEvaluate :: QC.Property
propMinutesEvaluate =
  QC.forAll (QC.chooseInt (1, 31536000)) $ \m ->
    evaluateRateT (RateMinutes m)
      QC.=== Right (secondsToNominalDiffTime (fromIntegral (m * 60)))

propHoursEvaluate :: QC.Property
propHoursEvaluate =
  QC.forAll (QC.chooseInt (1, 8760)) $ \h ->
    evaluateRateT (RateHours h)
      QC.=== Right (secondsToNominalDiffTime (fromIntegral (h * 3600)))

propDaysEvaluate :: QC.Property
propDaysEvaluate =
  QC.forAll (QC.chooseInt (1, 365)) $ \d ->
    evaluateRateT (RateDays d)
      QC.=== Right (secondsToNominalDiffTime (fromIntegral (d * 86400)))

propParserRoundTrip :: QC.Property
propParserRoundTrip =
  QC.forAll genRateExprText $ \(expr, input) ->
    parseRateText input QC.=== Right expr

genRateExprText :: QC.Gen (RateExprT, T.Text)
genRateExprText =
  QC.oneof
    [ genCase RateMinutes 31536000 "minute"
    , genCase RateHours 8760 "hour"
    , genCase RateDays 365 "day"
    ]

genCase :: (Int -> RateExprT) -> Int -> String -> QC.Gen (RateExprT, T.Text)
genCase constructor upperBound unitLabel = do
  n <- QC.chooseInt (1, upperBound)
  plural <- QC.elements [False, True]
  prefix <- QC.elements ["", " ", "\t", "\n"]
  suffix <- QC.elements ["", " ", "\t", "\n"]
  gap <- QC.elements [" ", "  ", "\t"]
  let token = unitLabel <> if plural then "s" else ""
      rendered = prefix <> "rate(" <> show n <> gap <> token <> ")" <> suffix
  pure (constructor n, T.pack rendered)
