{-# LANGUAGE OverloadedStrings #-}

module AWS.EventBridge.HoursSpec (tests) where

import AWS.EventBridge.Hours
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
  testGroup "hours"
    [ manualTests
    , propertyTests
    ]

manualTests :: TestTree
manualTests = testGroup "manual"
  [ testCase "parse *" $ parseHoursText "*" @?= Right AllHours
  , testCase "parse literal hour" $ parseHoursText "7" @?= Right (AtHour 7)
  , testCase "parse range" $ parseHoursText "8-12" @?= Right (RangeHour 8 12)
  , testCase "parse step" $ parseHoursText "3/4" @?= Right (StepHour 3 4)
  , testCase "parse union" $ do
      expr <- expectParseWith "hours" parseHoursText "1,12,18"
      result <- expectEvalWith "hours" evaluateHourT expr
      result @?= [1,12,18]
  , testCase "evaluate AllHours" $ do
      result <- expectEvalWith "hours" evaluateHourT AllHours
      result @?= [0..23]
  , testCase "invalid step start fails" $
      assertLeft (evaluateHourT (StepHour 25 2))
  , testCase "invalid step increment fails" $
      assertLeft (evaluateHourT (StepHour 3 0))
  ]

propertyTests :: TestTree
propertyTests = testGroup "properties"
  [ QC.testProperty "AllHours covers 0..23" propAllHoursRange
  , QC.testProperty "AtHour returns singleton" propAtHourSingleton
  , QC.testProperty "Generated expressions evaluate within bounds" propExpressionsWithinBounds
  , QC.testProperty "Parse of comma list matches sorted unique" propParseCommaSeparated
  , QC.testProperty "Invalid literal hour rejected" propInvalidHourRejected
  , QC.testProperty "Step hour forms arithmetic progression" propStepHourProgression
  ]

propAllHoursRange :: QC.Property
propAllHoursRange =
  case evaluateHourT AllHours of
    Left err -> QC.counterexample err False
    Right xs -> xs QC.=== [0..23]

propAtHourSingleton :: QC.Property
propAtHourSingleton =
  QC.forAll (QC.chooseInt (0, 23)) $ \h ->
    case evaluateHourT (AtHour h) of
      Left err -> QC.counterexample err False
      Right xs -> xs QC.=== [h]

propExpressionsWithinBounds :: QC.Property
propExpressionsWithinBounds =
  QC.forAll genHourExpr $ \expr ->
    case evaluateHourT expr of
      Left err -> QC.counterexample ("expr=" <> show expr <> ": " <> err) False
      Right xs ->
        let message = "expr=" <> show expr <> " xs=" <> show xs
        in QC.counterexample message (withinBoundsInt 0 23 xs && strictlyAscending xs)

propParseCommaSeparated :: QC.Property
propParseCommaSeparated =
  QC.forAll (QC.listOf1 (QC.chooseInt (0, 23))) $ \hoursList ->
    let txt = T.intercalate "," (map (T.pack . show) hoursList)
        expected = sort (nub hoursList)
    in case parseHoursText txt of
        Left err -> QC.counterexample err False
        Right expr ->
          case evaluateHourT expr of
            Left evalErr -> QC.counterexample evalErr False
            Right xs -> xs QC.=== expected

propInvalidHourRejected :: QC.Property
propInvalidHourRejected =
  QC.forAll invalidHour $ \h ->
    case evaluateHourT (AtHour h) of
      Left _ -> QC.property True
      Right xs ->
        QC.counterexample ("unexpected success: " <> show xs) False

propStepHourProgression :: QC.Property
propStepHourProgression =
  QC.forAll (QC.chooseInt (0, 23)) $ \start ->
    QC.forAll (QC.chooseInt (1, 12)) $ \step ->
      case evaluateHourT (StepHour start step) of
        Left err -> QC.counterexample err False
        Right xs ->
          QC.counterexample ("step output=" <> show xs)
            (withinBoundsInt 0 23 xs && isStepProgression start step xs)

genHourExpr :: QC.Gen HoursExprT
genHourExpr = QC.sized go
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
      [ pure AllHours
      , AtHour <$> QC.chooseInt (0, 23)
      ]
    genRange = do
      start <- QC.chooseInt (0, 23)
      end <- QC.chooseInt (start, 23)
      pure (RangeHour start end)
    genStep = do
      start <- QC.chooseInt (0, 23)
      step <- QC.chooseInt (1, 12)
      pure (StepHour start step)
    genUnion depth = do
      let sub = max 0 depth
      left <- go sub
      right <- go sub
      pure (UnionHour left right)

invalidHour :: QC.Gen Int
invalidHour = QC.oneof [QC.chooseInt (-24, -1), QC.chooseInt (24, 96)]

isStepProgression :: Int -> Int -> [Int] -> Bool
isStepProgression _ _ [] = True
isStepProgression start step (x0 : rest) =
  x0 == start && all (== step) (consecutiveDiffs x0 rest)
  where
    consecutiveDiffs _ [] = []
    consecutiveDiffs prev (y : ys) = (y - prev) : consecutiveDiffs y ys
