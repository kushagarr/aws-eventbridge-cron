{-# LANGUAGE OverloadedStrings #-}

module AWS.EventBridge.DayOfMonthSpec (tests) where

import AWS.EventBridge.DayOfMonth
import Data.Time.Calendar (fromGregorian, gregorianMonthLength)
import Data.Time.Calendar.WeekDate (toWeekDate)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck as QC
import TestSupport
  ( assertLeft
  , expectEvalWith
  , expectParseWith
  , strictlyAscending
  , withinBoundsInt
  )

tests :: TestTree
tests =
  testGroup "day-of-month"
    [ unitTests
    , propertyTests
    ]

unitTests :: TestTree
unitTests = testGroup "manual"
  [ testCase "parse L" $ parseDayOfMonthText "L" @?= Right DomLast
  , testCase "parse L-2" $ parseDayOfMonthText "L-2" @?= Right (DomLastOffset 2)
  , testCase "parse 15W" $ parseDayOfMonthText "15W" @?= Right (DomClosestWeekday 15)
    , testCase "parse numeric union" $ do
      expr <- expectParseWith "day-of-month" parseDayOfMonthText "1,5,10-12"
      result <- expectEvalWith "day-of-month" (evaluateDayOfMonthT 2024 2) expr
      result @?= [1,5,10,11,12]
  , testCase "parse ? cannot union" $ assertLeft (parseDayOfMonthText "?,5")
  , testCase "evaluate DomLast" $ do
      let dim = gregorianMonthLength 2025 3
      result <- expectEvalWith "day-of-month" (evaluateDayOfMonthT 2025 3) DomLast
      result @?= [dim]
  , testCase "closestWeekday Saturday at start" $
      closestWeekday 2025 3 1 @?= Just 3
  , testCase "closestWeekday Sunday at end" $
      closestWeekday 2021 1 31 @?= Just 29
  , testCase "lastWeekday handles weekend" $
      lastWeekday 2021 1 @?= 29
  , testCase "DomLastOffset clamps" $ do
      result <- expectEvalWith "day-of-month" (evaluateDayOfMonthT 2024 5) (DomLastOffset 2)
      result @?= [29]
  , testCase "DomLastOffset rejects overshoot" $
      assertLeft (evaluateDayOfMonthT 2024 2 (DomLastOffset 29))
  ]

propertyTests :: TestTree
propertyTests = testGroup "properties"
  [ QC.testProperty "closestWeekday stays within bounds and weekdays" propClosestWeekdayWithinBounds
  , QC.testProperty "closestWeekday rejects out-of-range days" propClosestWeekdayRejectsOutOfRange
  , QC.testProperty "lastWeekday produces weekday inside month" propLastWeekdayIsWeekday
  , QC.testProperty "DomLast evaluates to final day" propDomLastMatchesMonthLength
  , QC.testProperty "DomLastOffset yields expected day when within month" propDomLastOffsetWithinMonth
  , QC.testProperty "DomLastOffset rejects offsets >= month length" propDomLastOffsetTooLarge
  , QC.testProperty "Numeric expressions produce in-range ascending days" propNumericExpressionsWithinBounds
  , QC.testProperty "DomAny evaluates to empty set" propDomAnyEmpty
  ]

propClosestWeekdayWithinBounds :: QC.Property
propClosestWeekdayWithinBounds =
  QC.forAll genYearMonth $ \(y, m) ->
    QC.forAll (QC.chooseInt (1, gregorianMonthLength y m)) $ \d ->
      let result = closestWeekday y m d
          dim = gregorianMonthLength y m
      in case result of
          Nothing -> QC.counterexample "expected Just" False
          Just w ->
            let (_, _, dow) = toWeekDate (fromGregorian y m w)
            in QC.counterexample (failureMsg y m d w dow)
                 (w >= 1 && w <= dim && dow >= 1 && dow <= 5 && abs (w - d) <= 2)

propClosestWeekdayRejectsOutOfRange :: QC.Property
propClosestWeekdayRejectsOutOfRange =
  QC.forAll genYearMonth $ \(y, m) ->
    QC.forAll outOfRangeDay $ \d ->
      closestWeekday y m d QC.=== Nothing

propLastWeekdayIsWeekday :: QC.Property
propLastWeekdayIsWeekday =
  QC.forAll genYearMonth $ \(y, m) ->
    let lw = lastWeekday y m
        dim = gregorianMonthLength y m
        (_, _, dow) = toWeekDate (fromGregorian y m lw)
    in QC.counterexample (failureMsg y m dim lw dow)
         (lw >= 1 && lw <= dim && dow >= 1 && dow <= 5)

propDomLastMatchesMonthLength :: QC.Property
propDomLastMatchesMonthLength =
  QC.forAll genYearMonth $ \(y, m) ->
    case evaluateDayOfMonthT y m DomLast of
      Left err -> QC.counterexample err False
      Right xs ->
        let dim = gregorianMonthLength y m
        in QC.counterexample ("expected " <> show [dim] <> ", got " <> show xs)
             (xs == [dim])

propDomLastOffsetWithinMonth :: QC.Property
propDomLastOffsetWithinMonth =
  QC.forAll genDomLastOffsetWithin $ \(y, m, off) ->
    case evaluateDayOfMonthT y m (DomLastOffset off) of
      Left err -> QC.counterexample err False
      Right xs ->
        let dim = gregorianMonthLength y m
            expected = dim - off
        in QC.counterexample ("expected " <> show [expected] <> ", got " <> show xs)
             (xs == [expected])

propDomLastOffsetTooLarge :: QC.Property
propDomLastOffsetTooLarge =
  QC.forAll genDomLastOffsetTooLarge $ \(y, m, off) ->
    case evaluateDayOfMonthT y m (DomLastOffset off) of
      Left _ -> QC.property True
      Right xs ->
        let dim = gregorianMonthLength y m
        in QC.counterexample
             ("unexpected success " <> show xs <> " for month length " <> show dim <> " and offset " <> show off)
             False

propNumericExpressionsWithinBounds :: QC.Property
propNumericExpressionsWithinBounds =
  QC.forAll genYearMonth $ \(y, m) ->
    QC.forAll genNumericExpr $ \expr ->
      case evaluateDayOfMonthT y m expr of
        Left err -> QC.counterexample ("expr=" <> show expr <> ": " <> err) False
        Right xs ->
          let dim = gregorianMonthLength y m
              message = "expr=" <> show expr <> " xs=" <> show xs <> " dim=" <> show dim
          in QC.counterexample message (withinBoundsInt 1 dim xs && strictlyAscending xs)

propDomAnyEmpty :: QC.Property
propDomAnyEmpty =
  QC.forAll genYearMonth $ \(y, m) ->
    case evaluateDayOfMonthT y m DomAny of
      Left err -> QC.counterexample err False
      Right xs -> QC.counterexample ("xs=" <> show xs) (null xs)

genYearMonth :: QC.Gen (Integer, Int)
genYearMonth = do
  year <- QC.chooseInt (1900, 2100)
  month <- QC.chooseInt (1, 12)
  pure (fromIntegral year, month)

outOfRangeDay :: QC.Gen Int
outOfRangeDay = QC.oneof [QC.chooseInt (-5, 0), QC.chooseInt (32, 40)]

genDomLastOffsetWithin :: QC.Gen (Integer, Int, Int)
genDomLastOffsetWithin = do
  (y, m) <- genYearMonth
  let dim = gregorianMonthLength y m
  off <- QC.chooseInt (1, dim - 1)
  pure (y, m, off)

genDomLastOffsetTooLarge :: QC.Gen (Integer, Int, Int)
genDomLastOffsetTooLarge = do
  (y, m) <- genYearMonth
  let dim = gregorianMonthLength y m
  off <- QC.chooseInt (dim, dim + 10)
  pure (y, m, off)

genNumericExpr :: QC.Gen DayOfMonthExprT
genNumericExpr = QC.sized go
  where
    go n
      | n <= 1 = QC.oneof base
      | otherwise = QC.frequency
          [ (3, QC.oneof base)
          , (2, genRange)
          , (2, genStep)
          , (2, genUnion (n - 1))
          ]
    base =
      [ pure DomAll
      , DomAt <$> QC.chooseInt (1, 31)
      ]
    genRange = do
      start <- QC.chooseInt (1, 31)
      end <- QC.chooseInt (start, 31)
      pure (DomRange start end)
    genStep = do
      start <- QC.chooseInt (1, 31)
      step <- QC.chooseInt (1, 10)
      pure (DomStep start step)
    genUnion depth = do
      let sub = max 0 depth
      left <- go sub
      right <- go sub
      pure (DomUnion left right)

failureMsg :: Integer -> Int -> Int -> Int -> Int -> String
failureMsg y m d w dow =
  "y=" <> show y <> " m=" <> show m <> " requested=" <> show d <>
  " resolved=" <> show w <> " weekday=" <> show dow
