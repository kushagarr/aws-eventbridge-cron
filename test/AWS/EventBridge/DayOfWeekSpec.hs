{-# LANGUAGE OverloadedStrings #-}

module AWS.EventBridge.DayOfWeekSpec (tests) where

import AWS.EventBridge.DayOfWeek
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Time.Calendar (gregorianMonthLength, fromGregorian)
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
  testGroup "day-of-week"
    [ manualTests
    , propertyTests
    ]

manualTests :: TestTree
manualTests = testGroup "manual"
  [ testCase "parse *" $ parseDayOfWeekText "*" @?= Right DowAll
  , testCase "parse ?" $ parseDayOfWeekText "?" @?= Right DowAny
  , testCase "parse named literal" $ parseDayOfWeekText "Mon" @?= Right (DowAt 2)
  , testCase "parse numeric literal" $ parseDayOfWeekText "3" @?= Right (DowAt 3)
  , testCase "parse range" $ parseDayOfWeekText "MON-FRI" @?= Right (DowRange 2 6)
    , testCase "parse single-day range" $ parseDayOfWeekText "FRI-FRI" @?= Right (DowRange 6 6)
  , testCase "parse nth" $ parseDayOfWeekText "TUE#2" @?= Right (DowNth 3 2)
    , testCase "parse numeric nth" $ parseDayOfWeekText "5#1" @?= Right (DowNth 5 1)
  , testCase "parse union" $ parseDayOfWeekText "SUN,WED" @?= Right (DowUnion (DowAt 1) (DowAt 4))
    , testCase "parse named alias" $ parseDayOfWeekText "Thur" @?= Right (DowAt 5)
  , testCase "? cannot mix" $
      case parseDayOfWeekText "?,MON" of
        Left _ -> pure ()
        Right val -> fail ("unexpected success: " <> show val)
  , testCase "# cannot mix" $
      case parseDayOfWeekText "MON#1,FRI" of
        Left _ -> pure ()
        Right val -> fail ("unexpected success: " <> show val)
    , testCase "invalid day name fails" $
      case parseDayOfWeekText "FUNDAY" of
      Left _ -> pure ()
      Right val -> fail ("unexpected success: " <> show val)
    , testCase "evaluate MON in April 2025" $ do
      expr <- expectParseWith "dow" parseDayOfWeekText "MON"
      result <- expectEvalWith "dow" (evaluateDayOfWeekT 2025 4) expr
      result @?= [7,14,21,28]
  , testCase "nth occurrence absent yields empty" $ do
      expr <- expectParseWith "dow" parseDayOfWeekText "SUN#5"
      result <- expectEvalWith "dow" (evaluateDayOfWeekT 2025 2) expr
      result @?= []
  , testCase "nth occurrence resolves" $ do
      expr <- expectParseWith "dow" parseDayOfWeekText "THU#3"
      result <- expectEvalWith "dow" (evaluateDayOfWeekT 2025 5) expr
      result @?= [15]
      , testCase "union duplicates collapse" $ do
        expr <- expectParseWith "dow" parseDayOfWeekText "MON,mon"
        result <- expectEvalWith "dow" (evaluateDayOfWeekT 2025 5) expr
        result @?= [5,12,19,26]
  , testCase "invalid occurrence rejected" $
      assertLeft (evaluateDayOfWeekT 2025 5 (DowNth 3 6))
  , testCase "invalid day literal rejected" $
      assertLeft (evaluateDayOfWeekT 2025 5 (DowAt 9))
  ]

propertyTests :: TestTree
propertyTests = testGroup "properties"
  [ QC.testProperty "All days cover month" propAllCoversMonth
  , QC.testProperty "Any produces empty" propAnyEmpty
  , QC.testProperty "At day matches weekday" propAtMatchesWeekday
  , QC.testProperty "Range equals union of days" propRangeMatchesUnion
  , QC.testProperty "Range with same endpoints equals literal" propRangeDegenerateAsLiteral
  , QC.testProperty "Union combines unique days" propUnionCombines
  , QC.testProperty "Union with duplicates collapses" propUnionDeduplicates
  , QC.testProperty "Nth occurrence matches calendar" propNthMatchesCalendar
  , QC.testProperty "Generated expressions evaluate within bounds" propExpressionsWithinBounds
  , QC.testProperty "Named tokens parse case-insensitively" propNamedParseCaseInsensitive
  , QC.testProperty "Multiple # expressions rejected" propMultipleHashRejected
  ]

propAllCoversMonth :: QC.Property
propAllCoversMonth =
  QC.forAll genYearMonth $ \(y, m) ->
    case evaluateDayOfWeekT y m DowAll of
      Left err -> QC.counterexample err False
      Right xs -> xs QC.=== [1 .. gregorianMonthLength y m]

propAnyEmpty :: QC.Property
propAnyEmpty =
  QC.forAll genYearMonth $ \(y, m) ->
    case evaluateDayOfWeekT y m DowAny of
      Left err -> QC.counterexample err False
      Right xs -> xs QC.=== []

propAtMatchesWeekday :: QC.Property
propAtMatchesWeekday =
  QC.forAll genYearMonth $ \(y, m) ->
    QC.forAll (QC.chooseInt (1, 7)) $ \dow ->
      case evaluateDayOfWeekT y m (DowAt dow) of
        Left err -> QC.counterexample err False
        Right xs ->
          let msg = "y=" <> show y <> " m=" <> show m <> " dow=" <> show dow <> " xs=" <> show xs
          in QC.counterexample msg (all (\d -> awsDow y m d == dow) xs)

propRangeMatchesUnion :: QC.Property
propRangeMatchesUnion =
  QC.forAll genYearMonth $ \(y, m) ->
    QC.forAll genRangeBounds $ \(start, end) ->
      let expected = Set.toAscList (Set.fromList (concatMap (expectedDays y m) [start .. end]))
      in case evaluateDayOfWeekT y m (DowRange start end) of
          Left err -> QC.counterexample err False
          Right xs -> xs QC.=== expected

propUnionCombines :: QC.Property
propUnionCombines =
  QC.forAll genYearMonth $ \(y, m) ->
    QC.forAll genDayPair $ \(a, b) ->
      let expected = Set.toAscList (Set.fromList (expectedDays y m a ++ expectedDays y m b))
      in case evaluateDayOfWeekT y m (DowUnion (DowAt a) (DowAt b)) of
          Left err -> QC.counterexample err False
          Right xs -> xs QC.=== expected

propRangeDegenerateAsLiteral :: QC.Property
propRangeDegenerateAsLiteral =
  QC.forAll genYearMonth $ \(y, m) ->
    QC.forAll (QC.chooseInt (1, 7)) $ \dow ->
      case evaluateDayOfWeekT y m (DowRange dow dow) of
        Left err -> QC.counterexample err False
        Right xs ->
          case evaluateDayOfWeekT y m (DowAt dow) of
            Left err -> QC.counterexample err False
            Right expected -> xs QC.=== expected

propUnionDeduplicates :: QC.Property
propUnionDeduplicates =
  QC.forAll genYearMonth $ \(y, m) ->
    QC.forAll (QC.chooseInt (1, 7)) $ \dow ->
      let expr = DowUnion (DowAt dow) (DowAt dow)
      in case evaluateDayOfWeekT y m expr of
          Left err -> QC.counterexample err False
          Right xs ->
            case evaluateDayOfWeekT y m (DowAt dow) of
              Left err -> QC.counterexample err False
              Right expected -> xs QC.=== expected

propNthMatchesCalendar :: QC.Property
propNthMatchesCalendar =
  QC.forAll genYearMonth $ \(y, m) ->
    QC.forAll (QC.chooseInt (1, 7)) $ \dow ->
      QC.forAll (QC.chooseInt (1, 5)) $ \nth ->
        let occurrences = expectedDays y m dow
        in case evaluateDayOfWeekT y m (DowNth dow nth) of
            Left err -> QC.counterexample err False
            Right xs ->
              case drop (nth - 1) occurrences of
                (d : _) -> xs QC.=== [d]
                [] -> xs QC.=== []

propExpressionsWithinBounds :: QC.Property
propExpressionsWithinBounds =
  QC.forAll genYearMonth $ \(y, m) ->
    QC.forAll genDayOfWeekExpr $ \expr ->
      case evaluateDayOfWeekT y m expr of
        Left err -> QC.counterexample ("expr=" <> show expr <> ": " <> err) False
        Right xs ->
          let dim = gregorianMonthLength y m
              msg = "expr=" <> show expr <> " xs=" <> show xs
          in QC.counterexample msg (withinBoundsInt 1 dim xs && strictlyAscending xs)

propNamedParseCaseInsensitive :: QC.Property
propNamedParseCaseInsensitive =
  QC.forAll (QC.elements namedTokenCases) $ \(token, expected) ->
    case parseDayOfWeekText token of
      Left err -> QC.counterexample err False
      Right expr -> expr QC.=== expected

propMultipleHashRejected :: QC.Property
propMultipleHashRejected =
  QC.forAll genYearMonth $ \(y, m) ->
    let candidate = "MON#1,TUE#2"
    in case parseDayOfWeekText candidate of
         Left _ -> QC.property True
         Right expr ->
           let eval = evaluateDayOfWeekT y m expr
           in QC.counterexample ("unexpected success: " <> show expr <> ", eval=" <> show eval) False

-- Generators

genYearMonth :: QC.Gen (Integer, Int)
genYearMonth = do
  year <- QC.chooseInteger (1970, 2199)
  month <- QC.chooseInt (1, 12)
  pure (year, month)

genRangeBounds :: QC.Gen (Int, Int)
genRangeBounds = do
  start <- QC.chooseInt (1, 7)
  end <- QC.chooseInt (start, 7)
  pure (start, end)

genDayPair :: QC.Gen (Int, Int)
genDayPair = do
  a <- QC.chooseInt (1, 7)
  b <- QC.chooseInt (1, 7)
  pure (a, b)

genDayOfWeekExpr :: QC.Gen DayOfWeekExprT
genDayOfWeekExpr = QC.sized go
  where
    go n
      | n <= 1 = QC.oneof base
      | otherwise = QC.frequency
          [ (3, QC.oneof base)
          , (2, do
                start <- QC.chooseInt (1, 7)
                end <- QC.chooseInt (start, 7)
                pure (DowRange start end))
          , (2, do
                let sub = max 1 (n `div` 2)
                DowUnion <$> go sub <*> go sub)
          ]
    base =
      [ pure DowAny
      , pure DowAll
      , DowAt <$> QC.chooseInt (1, 7)
      , DowNth <$> QC.chooseInt (1, 7) <*> QC.chooseInt (1, 5)
      ]
    
namedTokenCases :: [(T.Text, DayOfWeekExprT)]
namedTokenCases =
  [ ("sun", DowAt 1)
  , ("MON", DowAt 2)
  , ("tue", DowAt 3)
  , ("Wed", DowAt 4)
  , ("thurs", DowAt 5)
  , ("FRI", DowAt 6)
  , ("Sat", DowAt 7)
  ]

-- Helpers

awsDow :: Integer -> Int -> Int -> Int
awsDow y m d =
  let (_, _, iso) = toWeekDate (fromGregorian y m d)
  in (iso `mod` 7) + 1

expectedDays :: Integer -> Int -> Int -> [Int]
expectedDays y m dow =
  [ dayOfMonth
  | dayOfMonth <- [1 .. gregorianMonthLength y m]
  , awsDow y m dayOfMonth == dow
  ]