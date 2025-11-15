{-# LANGUAGE OverloadedStrings #-}

module AWS.EventBridge.YearsSpec (tests) where

import AWS.EventBridge.Years
import Data.List (nub, sort)
import qualified Data.Set as Set
import qualified Data.Text as T
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck as QC
import TestSupport
  ( assertLeft
  , expectEvalEquals
  , expectEvalWith
  , expectParseWith
  , strictlyAscending
  , withinBoundsInt
  )

tests :: TestTree
tests =
  testGroup "years"
    [ manualTests
    , propertyTests
    ]

manualTests :: TestTree
manualTests = testGroup "manual"
  [ testCase "parse *" $ parseYearsText "*" @?= Right AllYears
  , testCase "parse literal year" $ parseYearsText "2025" @?= Right (AtYear 2025)
  , testCase "parse range" $ parseYearsText "1975-1980" @?= Right (RangeYear 1975 1980)
  , testCase "parse step" $ parseYearsText "2000/5" @?= Right (StepYear 2000 5)
  , testCase "parse union" $ do
      expr <- expectParseWith "years" parseYearsText "1970,2020,2199"
      result <- expectEvalWith "years" evaluateYearT expr
      result @?= [1970,2020,2199]
    , testCase "parse union with duplicates collapses" $ do
      expr <- expectParseWith "years" parseYearsText "1970,1970,2199"
      result <- expectEvalWith "years" evaluateYearT expr
      result @?= [1970,2199]
  , testCase "evaluate AllYears" $ expectEvalEquals [1970..2199] (evaluateYearT AllYears)
  , testCase "invalid step start fails" $
      assertLeft (evaluateYearT (StepYear 1969 2))
  , testCase "invalid step increment fails" $
      assertLeft (evaluateYearT (StepYear 1980 0))
  , testCase "literal out of range fails" $
      assertLeft (evaluateYearT (AtYear 2250))
    , testCase "descending range rejected" $
      assertLeft (evaluateYearT (RangeYear 2025 2020))
  ]

propertyTests :: TestTree
propertyTests = testGroup "properties"
  [ QC.testProperty "AllYears covers 1970..2199" propAllYearsRange
  , QC.testProperty "AtYear returns singleton" propAtYearSingleton
  , QC.testProperty "Generated expressions evaluate within bounds" propExpressionsWithinBounds
  , QC.testProperty "Parse of comma list matches sorted unique" propParseCommaSeparated
  , QC.testProperty "Invalid literal year rejected" propInvalidYearRejected
  , QC.testProperty "Step year forms arithmetic progression" propStepYearProgression
    , QC.testProperty "Range year evaluates inclusively" propRangeYearInclusive
    , QC.testProperty "Descending range rejected" propRangeYearRejectsDescending
    , QC.testProperty "Union year combines operands" propUnionYearCombines
  ]

propAllYearsRange :: QC.Property
propAllYearsRange =
  case evaluateYearT AllYears of
    Left err -> QC.counterexample err False
    Right xs -> xs QC.=== [1970..2199]

propAtYearSingleton :: QC.Property
propAtYearSingleton =
  QC.forAll (QC.chooseInt (1970, 2199)) $ \y ->
    case evaluateYearT (AtYear y) of
      Left err -> QC.counterexample err False
      Right xs -> xs QC.=== [y]

propExpressionsWithinBounds :: QC.Property
propExpressionsWithinBounds =
  QC.forAll genYearExpr $ \expr ->
    case evaluateYearT expr of
      Left err -> QC.counterexample ("expr=" <> show expr <> ": " <> err) False
      Right xs ->
        let message = "expr=" <> show expr <> " xs=" <> show xs
        in QC.counterexample message (withinBoundsInt 1970 2199 xs && strictlyAscending xs)

propParseCommaSeparated :: QC.Property
propParseCommaSeparated =
  QC.forAll (QC.listOf1 (QC.chooseInt (1970, 2199))) $ \years ->
    let txt = T.intercalate "," (map (T.pack . show) years)
        expected = sort (nub years)
    in case parseYearsText txt of
        Left err -> QC.counterexample err False
        Right expr ->
          case evaluateYearT expr of
            Left evalErr -> QC.counterexample evalErr False
            Right xs -> xs QC.=== expected

propInvalidYearRejected :: QC.Property
propInvalidYearRejected =
  QC.forAll invalidYear $ \y ->
    case evaluateYearT (AtYear y) of
      Left _ -> QC.property True
      Right xs -> QC.counterexample ("unexpected success: " <> show xs) False

propRangeYearInclusive :: QC.Property
propRangeYearInclusive =
  QC.forAll (QC.chooseInt (1970, 2199)) $ \start ->
    QC.forAll (QC.chooseInt (start, 2199)) $ \end ->
      case evaluateYearT (RangeYear start end) of
        Left err -> QC.counterexample err False
        Right xs -> xs QC.=== [start..end]

propRangeYearRejectsDescending :: QC.Property
propRangeYearRejectsDescending =
  QC.forAll (QC.chooseInt (1971, 2199)) $ \start ->
    QC.forAll (QC.chooseInt (1970, start - 1)) $ \end ->
      case evaluateYearT (RangeYear start end) of
        Left _ -> QC.property True
        Right xs -> QC.counterexample ("unexpected success: " <> show xs) False

propStepYearProgression :: QC.Property
propStepYearProgression =
  QC.forAll (QC.chooseInt (1970, 2199)) $ \start ->
    QC.forAll (QC.chooseInt (1, 40)) $ \step ->
      case evaluateYearT (StepYear start step) of
        Left err -> QC.counterexample err False
        Right xs ->
          QC.counterexample ("step output=" <> show xs)
            (withinBoundsInt 1970 2199 xs && isStepProgression start step xs)

propUnionYearCombines :: QC.Property
propUnionYearCombines =
  QC.forAll genYearExpr $ \lhs ->
    QC.forAll genYearExpr $ \rhs ->
      case ( evaluateYearT lhs
           , evaluateYearT rhs
           , evaluateYearT (UnionYear lhs rhs)
           ) of
        (Right xs, Right ys, Right zs) ->
          let expected = Set.toAscList (Set.fromList (xs ++ ys))
              msg = "lhs=" <> show lhs <> " rhs=" <> show rhs <> " expected=" <> show expected <> " actual=" <> show zs
          in QC.counterexample msg (zs == expected)
        (Left err, _, _) -> QC.counterexample ("lhs failed: " <> err) False
        (_, Left err, _) -> QC.counterexample ("rhs failed: " <> err) False
        (_, _, Left err) -> QC.counterexample ("union failed: " <> err) False

-- Generators

genYearExpr :: QC.Gen YearsExprT
genYearExpr = QC.sized go
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
      [ pure AllYears
      , AtYear <$> QC.chooseInt (1970, 2199)
      ]
    genRange = do
      start <- QC.chooseInt (1970, 2199)
      end <- QC.chooseInt (start, 2199)
      pure (RangeYear start end)
    genStep = do
      start <- QC.chooseInt (1970, 2199)
      step <- QC.chooseInt (1, 40)
      pure (StepYear start step)
    genUnion depth = do
      let sub = max 0 depth
      left <- go sub
      right <- go sub
      pure (UnionYear left right)

invalidYear :: QC.Gen Int
invalidYear = QC.oneof [QC.chooseInt (1800, 1969), QC.chooseInt (2200, 2300)]

-- Helpers

isStepProgression :: Int -> Int -> [Int] -> Bool
isStepProgression _ _ [] = True
isStepProgression _ _ [_] = True
isStepProgression start step xs@(x0 : _) =
  x0 == start && all (== step) (zipWith (-) (tail xs) xs)
