{-# LANGUAGE OverloadedStrings #-}

module AWS.EventBridge.CronSpec (tests) where

import AWS.EventBridge.Cron
import AWS.EventBridge.DayOfMonth (DayOfMonthExprT (..), evaluateDayOfMonthT)
import AWS.EventBridge.DayOfWeek (DayOfWeekExprT (..), evaluateDayOfWeekT)
import Data.Time (UTCTime (..), addUTCTime, secondsToNominalDiffTime)
import Data.Time.Calendar (fromGregorian, toGregorian)
import Data.Time.LocalTime (TimeOfDay (..), timeOfDayToTime)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
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
      _ <- expectParseWith "cron" parseCronText "cron(0 0 ? * 2#1 2025)"
      pure ()
  , testCase "next run times rate" $ do
      let base = UTCTime (fromGregorian 2025 11 16) (timeOfDayToTime (TimeOfDay 9 0 0))
      rateExpr <- expectParseWith "rate" parseCronText "rate(5 minutes)"
      nextRunTimes base 3 rateExpr
        @?= Right
          [ base
          , addMinutes base 5
          , addMinutes base 10
          ]
  , testCase "next run times one-time" $ do
      let base = UTCTime (fromGregorian 2025 11 16) (timeOfDayToTime (TimeOfDay 9 0 0))
      oneTimeExpr <- expectParseWith "one-time" parseCronText "at(2025-11-16T09:30:00)"
      nextRunTimes base 1 oneTimeExpr
        @?= Right [UTCTime (fromGregorian 2025 11 16) (timeOfDayToTime (TimeOfDay 9 30 0))]
  , testCase "next run times cron" $ do
      let base = UTCTime (fromGregorian 2025 11 16) (timeOfDayToTime (TimeOfDay 9 0 0))
      cronExpr <- expectParseWith "cron" parseCronText "cron(0/15 9 ? NOV SUN 2025)"
      nextRunTimes base 4 cronExpr
        @?= Right
          [ UTCTime (fromGregorian 2025 11 16) (timeOfDayToTime (TimeOfDay 9 0 0))
          , UTCTime (fromGregorian 2025 11 16) (timeOfDayToTime (TimeOfDay 9 15 0))
          , UTCTime (fromGregorian 2025 11 16) (timeOfDayToTime (TimeOfDay 9 30 0))
          , UTCTime (fromGregorian 2025 11 16) (timeOfDayToTime (TimeOfDay 9 45 0))
          ]
  , testCase "cron day-of-month evaluated when day-of-week '?'" $ do
      let base = UTCTime (fromGregorian 2025 11 1) (timeOfDayToTime (TimeOfDay 8 0 0))
      cronExpr <- expectParseWith "cron" parseCronText "cron(0 9 5 NOV ? 2025)"
      nextRunTimes base 2 cronExpr
        @?= Right
          [ UTCTime (fromGregorian 2025 11 5) (timeOfDayToTime (TimeOfDay 9 0 0))
          ]
  , testCase "cron skips intraday times earlier than base" $ do
      let base = UTCTime (fromGregorian 2025 11 16) (timeOfDayToTime (TimeOfDay 9 7 30))
      cronExpr <- expectParseWith "cron" parseCronText "cron(0/15 9 ? NOV SUN 2025)"
      nextRunTimes base 3 cronExpr
        @?= Right
          [ UTCTime (fromGregorian 2025 11 16) (timeOfDayToTime (TimeOfDay 9 15 0))
          , UTCTime (fromGregorian 2025 11 16) (timeOfDayToTime (TimeOfDay 9 30 0))
          , UTCTime (fromGregorian 2025 11 16) (timeOfDayToTime (TimeOfDay 9 45 0))
          ]
  , testCase "cron spans multiple allowed years" $ do
      let base = UTCTime (fromGregorian 2026 12 31) (timeOfDayToTime (TimeOfDay 23 0 0))
      cronExpr <- expectParseWith "cron" parseCronText "cron(0 0 1 JAN ? 2027-2028)"
      nextRunTimes base 2 cronExpr
        @?= Right
          [ UTCTime (fromGregorian 2027 1 1) (timeOfDayToTime (TimeOfDay 0 0 0))
          , UTCTime (fromGregorian 2028 1 1) (timeOfDayToTime (TimeOfDay 0 0 0))
          ]
  , testCase "invalid day fields combination" $ do
      cronExpr <- expectParseWith "cron" parseCronText "cron(0 0 1 * 2 2025)"
      nextRunTimes (UTCTime (fromGregorian 2025 1 1) 0) 1 cronExpr
        @?= Left "day-of-month and day-of-week fields must use '?' in exactly one position"
  , testCase "invalid rate" $ do
      rateExpr <- expectParseWith "rate" parseCronText "rate(0 minutes)"
      let base = UTCTime (fromGregorian 2025 1 1) 0
      nextRunTimes base 1 rateExpr
        @?= Left "invalid rate minutes: 0 (expected 1..31536000)"
  , testCase "invalid one-time" $
      assertLeft (parseCronText "at(2025-13-01T00:00:00)")
  ]

propertyTests :: TestTree
propertyTests = testGroup "properties"
  [ QC.testProperty "rate schedule monotonic" propRateMonotonic
  , QC.testProperty "schedule kind round-trips for cron" propScheduleKindCron
  , QC.testProperty "schedule kind round-trips for rate" propScheduleKindRate
  , QC.testProperty "schedule kind identifies one-time" propScheduleKindOneTime
  , QC.testProperty "isRecurring agrees with schedule kind" propIsRecurring
  , QC.testProperty "cron returns at most requested" propCronLimit
  , QC.testProperty "cron schedule monotonic and >= base" propCronMonotonic
  , QC.testProperty "cron with '?' dom relies on day-of-week" propCronDomQuestionUsesDow
  , QC.testProperty "cron with '?' dow relies on day-of-month" propCronDowQuestionUsesDom
  ]

propScheduleKindCron :: QC.Property
propScheduleKindCron =
  let exprText = "cron(0 0 ? NOV MON 2025)"
  in case parseCronText exprText of
      Left err -> QC.counterexample ("cron parse failed: " <> err) False
      Right expr ->
        QC.counterexample (show (scheduleKind expr)) (scheduleKind expr QC.=== CronSchedule)

propScheduleKindRate :: QC.Property
propScheduleKindRate =
  let exprText = "rate(15 minutes)"
  in case parseCronText exprText of
      Left err -> QC.counterexample ("rate parse failed: " <> err) False
      Right expr ->
        QC.counterexample (show (scheduleKind expr)) (scheduleKind expr QC.=== RateSchedule)

propScheduleKindOneTime :: QC.Property
propScheduleKindOneTime =
  let exprText = "at(2025-11-16T09:30:00)"
  in case parseCronText exprText of
      Left err -> QC.counterexample ("at parse failed: " <> err) False
      Right expr ->
        QC.counterexample (show (scheduleKind expr)) (scheduleKind expr QC.=== OneTimeSchedule)

propIsRecurring :: QC.Property
propIsRecurring =
  QC.conjoin
    [ QC.counterexample "cron should be recurring" cronRecurring
    , QC.counterexample "rate should be recurring" rateRecurring
    , QC.counterexample "one-time should not be recurring" oneTimeNonRecurring
    ]
  where
    cronRecurring =
      case parseCronText "cron(0 12 ? JAN MON 2025)" of
        Right expr -> isRecurring expr
        Left _ -> False
    rateRecurring =
      case parseCronText "rate(1 hour)" of
        Right expr -> isRecurring expr
        Left _ -> False
    oneTimeNonRecurring =
      case parseCronText "at(2025-11-16T09:30:00)" of
        Right expr -> not (isRecurring expr)
        Left _ -> False

propRateMonotonic :: QC.Property
propRateMonotonic =
  QC.forAll (QC.chooseInt (1, 60)) $ \minutes ->
    let base = UTCTime (fromGregorian 2025 1 1) (timeOfDayToTime (TimeOfDay 0 0 0))
     in case parseCronText ("rate(" <> T.pack (show minutes) <> " minutes)") of
          Left err -> QC.counterexample ("rate parse failed: " <> err) False
          Right rateExpr ->
            case nextRunTimes base 5 rateExpr of
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
            case nextRunTimes base limit cronExpr of
              Left err -> QC.counterexample ("cron evaluation failed: " <> err) False
              Right ts -> QC.counterexample (show ts) (length ts <= limit)

propCronMonotonic :: QC.Property
propCronMonotonic =
  QC.forAll (QC.chooseInt (1, 5)) $ \limit ->
    let base = UTCTime (fromGregorian 2025 11 16) (timeOfDayToTime (TimeOfDay 9 5 0))
     in case parseCronText "cron(0/15 9 ? NOV SUN 2025)" of
          Left err -> QC.counterexample ("cron parse failed: " <> err) False
          Right cronExpr ->
            case nextRunTimes base limit cronExpr of
              Left err -> QC.counterexample ("cron evaluation failed: " <> err) False
              Right ts -> QC.counterexample (show ts) (nonDecreasing ts && all (>= base) ts)
  where
    nonDecreasing xs = and (zipWith (<=) xs (drop 1 xs))

propCronDomQuestionUsesDow :: QC.Property
propCronDomQuestionUsesDow =
  QC.forAll genCronDomQuestionCase $ \(exprText, year, month, dowExpr, base, limit) ->
    case parseCronText exprText of
      Left err -> QC.counterexample ("cron parse failed: " <> err) False
      Right cronExpr ->
        case evaluateDayOfWeekT year month dowExpr of
          Left err -> QC.counterexample ("day-of-week evaluation failed: " <> err) False
          Right allowedDays ->
            case nextRunTimes base limit cronExpr of
              Left err -> QC.counterexample ("cron evaluation failed: " <> err) False
              Right ts ->
                let days = map (thirdOf . toGregorian . utctDay) ts
                    monthsOk = all ((== month) . secondOf . toGregorian . utctDay) ts
                    yearsOk = all ((== year) . firstOf . toGregorian . utctDay) ts
                 in QC.counterexample (show ts)
                      (monthsOk && yearsOk && all (`elem` allowedDays) days)

propCronDowQuestionUsesDom :: QC.Property
propCronDowQuestionUsesDom =
  QC.forAll genCronDowQuestionCase $ \(exprText, year, month, domExpr, base, limit) ->
    case parseCronText exprText of
      Left err -> QC.counterexample ("cron parse failed: " <> err) False
      Right cronExpr ->
        case evaluateDayOfMonthT year month domExpr of
          Left err -> QC.counterexample ("day-of-month evaluation failed: " <> err) False
          Right allowedDays ->
            case nextRunTimes base limit cronExpr of
              Left err -> QC.counterexample ("cron evaluation failed: " <> err) False
              Right ts ->
                let days = map (thirdOf . toGregorian . utctDay) ts
                    monthsOk = all ((== month) . secondOf . toGregorian . utctDay) ts
                    yearsOk = all ((== year) . firstOf . toGregorian . utctDay) ts
                 in QC.counterexample (show ts)
                      (monthsOk && yearsOk && all (`elem` allowedDays) days)

addMinutes :: UTCTime -> Integer -> UTCTime
addMinutes t minutes = addUTCTime (secondsToNominalDiffTime (fromIntegral (minutes * 60))) t

genCronDomQuestionCase :: QC.Gen (T.Text, Integer, Int, DayOfWeekExprT, UTCTime, Int)
genCronDomQuestionCase = do
  minute <- QC.chooseInt (0, 59)
  hour <- QC.chooseInt (0, 23)
  month <- QC.chooseInt (1, 12)
  year <- QC.chooseInt (2025, 2030)
  dowExpr <- genDowExpr
  limit <- QC.chooseInt (1, 5)
  let exprText = renderCron minute hour "?" (T.pack (show month)) (renderDowExpr dowExpr) (T.pack (show year))
      base = UTCTime (fromGregorian (toInteger year) month 1) (timeOfDayToTime (TimeOfDay 0 0 0))
  pure (exprText, toInteger year, month, dowExpr, base, limit)

genCronDowQuestionCase :: QC.Gen (T.Text, Integer, Int, DayOfMonthExprT, UTCTime, Int)
genCronDowQuestionCase = do
  minute <- QC.chooseInt (0, 59)
  hour <- QC.chooseInt (0, 23)
  month <- QC.chooseInt (1, 12)
  year <- QC.chooseInt (2025, 2030)
  domExpr <- genDomExpr
  limit <- QC.chooseInt (1, 5)
  let exprText = renderCron minute hour (renderDomExpr domExpr) (T.pack (show month)) "?" (T.pack (show year))
      base = UTCTime (fromGregorian (toInteger year) month 1) (timeOfDayToTime (TimeOfDay 0 0 0))
  pure (exprText, toInteger year, month, domExpr, base, limit)

genDowExpr :: QC.Gen DayOfWeekExprT
genDowExpr = QC.oneof
  [ DowAt <$> QC.chooseInt (1, 7)
  , do
      start <- QC.chooseInt (1, 7)
      end <- QC.chooseInt (start, 7)
      pure (if start == end then DowAt start else DowRange start end)
  ]

genDomExpr :: QC.Gen DayOfMonthExprT
genDomExpr = QC.oneof
  [ DomAt <$> QC.chooseInt (1, 28)
  , do
      start <- QC.chooseInt (1, 28)
      end <- QC.chooseInt (start, 28)
      pure (if start == end then DomAt start else DomRange start end)
  ]

renderCron :: Int -> Int -> T.Text -> T.Text -> T.Text -> T.Text -> T.Text
renderCron minute hour domPart monthPart dowPart yearPart =
  T.concat
    [ "cron("
    , T.pack (show minute)
    , " "
    , T.pack (show hour)
    , " "
    , domPart
    , " "
    , monthPart
    , " "
    , dowPart
    , " "
    , yearPart
    , ")"
    ]

renderDowExpr :: DayOfWeekExprT -> T.Text
renderDowExpr DowAny = "?"
renderDowExpr DowAll = "*"
renderDowExpr (DowAt d) = T.pack (show d)
renderDowExpr (DowRange a b) = if a == b then T.pack (show a) else T.concat [T.pack (show a), "-", T.pack (show b)]
renderDowExpr (DowNth d n) = T.concat [T.pack (show d), "#", T.pack (show n)]
renderDowExpr (DowUnion a b) = T.concat [renderDowExpr a, ",", renderDowExpr b]

renderDomExpr :: DayOfMonthExprT -> T.Text
renderDomExpr DomAny = "?"
renderDomExpr DomAll = "*"
renderDomExpr (DomAt d) = T.pack (show d)
renderDomExpr (DomRange a b) = if a == b then T.pack (show a) else T.concat [T.pack (show a), "-", T.pack (show b)]
renderDomExpr (DomStep a b) = T.concat [T.pack (show a), "/", T.pack (show b)]
renderDomExpr (DomUnion a b) = T.concat [renderDomExpr a, ",", renderDomExpr b]
renderDomExpr DomLast = "L"
renderDomExpr (DomLastOffset n) = T.concat ["L-", T.pack (show n)]
renderDomExpr (DomClosestWeekday d) = T.concat [T.pack (show d), "W"]
renderDomExpr DomLastWeekday = "LW"

firstOf :: (a, b, c) -> a
firstOf (x, _, _) = x

secondOf :: (a, b, c) -> b
secondOf (_, y, _) = y

thirdOf :: (a, b, c) -> c
thirdOf (_, _, z) = z