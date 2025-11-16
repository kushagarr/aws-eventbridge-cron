{-# LANGUAGE OverloadedStrings #-}

module AWS.EventBridge.MinutesSpec (tests) where

import AWS.EventBridge.Minutes
import Data.List (nub, sort)
import qualified Data.Text as T
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
  testGroup "minutes"
    [ manualTests
    , propertyTests
    ]

manualTests :: TestTree
manualTests = testGroup "manual"
  [ testCase "parse *" $ parseMinutesText "*" @?= Right AllMinutes
  , testCase "parse literal minute" $ parseMinutesText "5" @?= Right (AtMinute 5)
  , testCase "parse range" $ parseMinutesText "10-12" @?= Right (RangeMinute 10 12)
  , testCase "parse step" $ parseMinutesText "5/15" @?= Right (StepMinute 5 15)
    , testCase "parse union" $ do
      expr <- expectParseWith "minutes" parseMinutesText "0,15,30"
      result <- expectEvalWith "minutes" evaluateMinuteT expr
      result @?= [0,15,30]
    , testCase "evaluate AllMinutes" $ do
      result <- expectEvalWith "minutes" evaluateMinuteT AllMinutes
      result @?= [0..59]
  , testCase "invalid step start fails" $
      assertLeft (evaluateMinuteT (StepMinute 75 5))
  , testCase "invalid step increment fails" $
      assertLeft (evaluateMinuteT (StepMinute 10 0))
  ]

propertyTests :: TestTree
propertyTests = testGroup "properties"
  [ QC.testProperty "AllMinutes covers 0..59" propAllMinutesRange
  , QC.testProperty "AtMinute returns singleton" propAtMinuteSingleton
  , QC.testProperty "Generated expressions evaluate within bounds" propExpressionsWithinBounds
  , QC.testProperty "Parse of comma list matches sorted unique" propParseCommaSeparated
  , QC.testProperty "Invalid literal minute rejected" propInvalidMinuteRejected
  , QC.testProperty "Step minute forms arithmetic progression" propStepMinuteProgression
  ]

propAllMinutesRange :: QC.Property
propAllMinutesRange =
  case evaluateMinuteT AllMinutes of
    Left err -> QC.counterexample err False
    Right xs -> xs QC.=== [0..59]

propAtMinuteSingleton :: QC.Property
propAtMinuteSingleton =
  QC.forAll (QC.chooseInt (0, 59)) $ \m ->
    case evaluateMinuteT (AtMinute m) of
      Left err -> QC.counterexample err False
      Right xs -> xs QC.=== [m]

propExpressionsWithinBounds :: QC.Property
propExpressionsWithinBounds =
  QC.forAll genMinuteExpr $ \expr ->
    case evaluateMinuteT expr of
      Left err -> QC.counterexample ("expr=" <> show expr <> ": " <> err) False
      Right xs ->
        let message = "expr=" <> show expr <> " xs=" <> show xs
        in QC.counterexample message (withinBoundsInt 0 59 xs && strictlyAscending xs)

propParseCommaSeparated :: QC.Property
propParseCommaSeparated =
  QC.forAll (QC.listOf1 (QC.chooseInt (0, 59))) $ \mins ->
    let txt = T.intercalate "," (map (T.pack . show) mins)
        expected = sort (nub mins)
    in case parseMinutesText txt of
        Left err -> QC.counterexample err False
        Right expr ->
          case evaluateMinuteT expr of
            Left evalErr -> QC.counterexample evalErr False
            Right xs -> xs QC.=== expected

propInvalidMinuteRejected :: QC.Property
propInvalidMinuteRejected =
  QC.forAll invalidMinute $ \m ->
    case evaluateMinuteT (AtMinute m) of
      Left _ -> QC.property True
      Right xs ->
        QC.counterexample ("unexpected success: " <> show xs) False

propStepMinuteProgression :: QC.Property
propStepMinuteProgression =
  QC.forAll (QC.chooseInt (0, 59)) $ \start ->
    QC.forAll (QC.chooseInt (1, 30)) $ \step ->
      case evaluateMinuteT (StepMinute start step) of
        Left err -> QC.counterexample err False
        Right xs ->
          QC.counterexample ("step output=" <> show xs)
            (withinBoundsInt 0 59 xs && isStepProgression start step xs)

genMinuteExpr :: QC.Gen MinutesExprT
genMinuteExpr = QC.sized go
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
      [ pure AllMinutes
      , AtMinute <$> QC.chooseInt (0, 59)
      ]
    genRange = do
      start <- QC.chooseInt (0, 59)
      end <- QC.chooseInt (start, 59)
      pure (RangeMinute start end)
    genStep = do
      start <- QC.chooseInt (0, 59)
      step <- QC.chooseInt (1, 30)
      pure (StepMinute start step)
    genUnion depth = do
      let sub = max 0 depth
      left <- go sub
      right <- go sub
      pure (UnionMinute left right)

invalidMinute :: QC.Gen Int
invalidMinute = QC.oneof [QC.chooseInt (-30, -1), QC.chooseInt (60, 120)]

isStepProgression :: Int -> Int -> [Int] -> Bool
isStepProgression _ _ [] = True
isStepProgression start step (x0 : rest) =
  x0 == start && all (== step) (consecutiveDiffs x0 rest)
  where
    consecutiveDiffs _ [] = []
    consecutiveDiffs prev (y : ys) = (y - prev) : consecutiveDiffs y ys
