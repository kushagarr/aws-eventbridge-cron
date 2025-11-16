{-# LANGUAGE OverloadedStrings #-}

module AWS.EventBridge.MonthsSpec (tests) where

import AWS.EventBridge.Months
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
  testGroup "months"
    [ manualTests
    , propertyTests
    ]

manualTests :: TestTree
manualTests = testGroup "manual"
  [ testCase "parse *" $ parseMonthsText "*" @?= Right AllMonths
  , testCase "parse numeric literal" $ parseMonthsText "3" @?= Right (AtMonth 3)
  , testCase "parse month name" $ parseMonthsText "Feb" @?= Right (AtMonth 2)
  , testCase "parse range" $ parseMonthsText "3-5" @?= Right (RangeMonth 3 5)
  , testCase "parse step" $ parseMonthsText "6/2" @?= Right (StepMonth 6 2)
  , testCase "parse mixed union" $ do
      expr <- expectParseWith "months" parseMonthsText "Jan,4,Aug"
      result <- expectEvalWith "months" evaluateMonthT expr
      result @?= [1,4,8]
  , testCase "parse union with duplicates collapses" $ do
      expr <- expectParseWith "months" parseMonthsText "Jan,jan,1"
      result <- expectEvalWith "months" evaluateMonthT expr
      result @?= [1]
  , testCase "evaluate AllMonths" $ expectEvalEquals [1..12] (evaluateMonthT AllMonths)
  , testCase "invalid month literal fails" $ assertLeft (evaluateMonthT (AtMonth 20))
  , testCase "invalid month name fails" $
      case parseMonthsText "Foo" of
        Left _ -> pure ()
        Right val -> fail ("unexpected success: " <> show val)
  , testCase "descending range rejected" $
      assertLeft (evaluateMonthT (RangeMonth 5 3))
  , testCase "step start out of range fails" $
      assertLeft (evaluateMonthT (StepMonth 0 2))
  , testCase "step increment zero fails" $
      assertLeft (evaluateMonthT (StepMonth 5 0))
  ]

propertyTests :: TestTree
propertyTests = testGroup "properties"
  [ QC.testProperty "AllMonths covers 1..12" propAllMonthsRange
  , QC.testProperty "AtMonth returns singleton" propAtMonthSingleton
  , QC.testProperty "Generated expressions evaluate within bounds" propExpressionsWithinBounds
  , QC.testProperty "Parse of comma list matches sorted unique" propParseCommaSeparated
  , QC.testProperty "Named months parse to expected indices" propNamedMonthsParse
  , QC.testProperty "Invalid literal month rejected" propInvalidMonthRejected
  , QC.testProperty "Range month evaluates inclusively" propRangeMonthInclusive
  , QC.testProperty "Descending range rejected" propRangeMonthRejectsDescending
  , QC.testProperty "Step month forms arithmetic progression" propStepMonthProgression
  , QC.testProperty "Union month combines operands" propUnionMonthCombines
  ]

propAllMonthsRange :: QC.Property
propAllMonthsRange =
  case evaluateMonthT AllMonths of
    Left err -> QC.counterexample err False
    Right xs -> xs QC.=== [1..12]

propAtMonthSingleton :: QC.Property
propAtMonthSingleton =
  QC.forAll (QC.chooseInt (1, 12)) $ \m ->
    case evaluateMonthT (AtMonth m) of
      Left err -> QC.counterexample err False
      Right xs -> xs QC.=== [m]

propExpressionsWithinBounds :: QC.Property
propExpressionsWithinBounds =
  QC.forAll genMonthExpr $ \expr ->
    case evaluateMonthT expr of
      Left err -> QC.counterexample ("expr=" <> show expr <> ": " <> err) False
      Right xs ->
        let message = "expr=" <> show expr <> " xs=" <> show xs
        in QC.counterexample message (withinBoundsInt 1 12 xs && strictlyAscending xs)

propParseCommaSeparated :: QC.Property
propParseCommaSeparated =
  QC.forAll (QC.listOf1 genMonthTokenText) $ \monthsTxt ->
    let txt = T.intercalate "," monthsTxt
        expected = sort (nub (map parseTokenUnsafe monthsTxt))
    in case parseMonthsText txt of
        Left err -> QC.counterexample err False
        Right expr ->
          case evaluateMonthT expr of
            Left evalErr -> QC.counterexample evalErr False
            Right xs -> xs QC.=== expected

propNamedMonthsParse :: QC.Property
propNamedMonthsParse =
  QC.forAll genNamedMonthText $ \nameTxt ->
    case parseMonthsText nameTxt of
      Left err -> QC.counterexample err False
      Right expr ->
        case expr of
          AtMonth m ->
            let expected = parseTokenUnsafe nameTxt
            in QC.counterexample ("expected " <> show expected <> ", got " <> show m) (m == expected)
          other -> QC.counterexample ("expected AtMonth, got " <> show other) False

propInvalidMonthRejected :: QC.Property
propInvalidMonthRejected =
  QC.forAll invalidMonth $ \m ->
    case evaluateMonthT (AtMonth m) of
      Left _ -> QC.property True
      Right xs -> QC.counterexample ("unexpected success: " <> show xs) False

propRangeMonthInclusive :: QC.Property
propRangeMonthInclusive =
  QC.forAll (QC.chooseInt (1, 12)) $ \start ->
    QC.forAll (QC.chooseInt (start, 12)) $ \end ->
      case evaluateMonthT (RangeMonth start end) of
        Left err -> QC.counterexample err False
        Right xs -> xs QC.=== [start..end]

propRangeMonthRejectsDescending :: QC.Property
propRangeMonthRejectsDescending =
  QC.forAll (QC.chooseInt (2, 12)) $ \start ->
    QC.forAll (QC.chooseInt (1, start - 1)) $ \end ->
      case evaluateMonthT (RangeMonth start end) of
        Left _ -> QC.property True
        Right xs -> QC.counterexample ("unexpected success: " <> show xs) False

propStepMonthProgression :: QC.Property
propStepMonthProgression =
  QC.forAll (QC.chooseInt (1, 12)) $ \start ->
    QC.forAll (QC.chooseInt (1, 6)) $ \step ->
      case evaluateMonthT (StepMonth start step) of
        Left err -> QC.counterexample err False
        Right xs ->
          QC.counterexample ("step output=" <> show xs)
            (withinBoundsInt 1 12 xs && isStepProgression start step xs)

propUnionMonthCombines :: QC.Property
propUnionMonthCombines =
  QC.forAll genMonthExpr $ \lhs ->
    QC.forAll genMonthExpr $ \rhs ->
      case ( evaluateMonthT lhs
           , evaluateMonthT rhs
           , evaluateMonthT (UnionMonth lhs rhs)
           ) of
        (Right xs, Right ys, Right zs) ->
          let expected = Set.toAscList (Set.fromList (xs ++ ys))
              msg = "lhs=" <> show lhs <> " rhs=" <> show rhs <> " expected=" <> show expected <> " actual=" <> show zs
          in QC.counterexample msg (zs == expected)
        (Left err, _, _) -> QC.counterexample ("lhs failed: " <> err) False
        (_, Left err, _) -> QC.counterexample ("rhs failed: " <> err) False
        (_, _, Left err) -> QC.counterexample ("union failed: " <> err) False

-- Generators

genMonthExpr :: QC.Gen MonthsExprT
genMonthExpr = QC.sized go
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
      [ pure AllMonths
      , AtMonth <$> QC.chooseInt (1, 12)
      ]
    genRange = do
      start <- QC.chooseInt (1, 12)
      end <- QC.chooseInt (start, 12)
      pure (RangeMonth start end)
    genStep = do
      start <- QC.chooseInt (1, 12)
      step <- QC.chooseInt (1, 6)
      pure (StepMonth start step)
    genUnion depth = do
      let sub = max 0 depth
      left <- go sub
      right <- go sub
      pure (UnionMonth left right)

genMonthTokenText :: QC.Gen T.Text
genMonthTokenText = QC.oneof
  [ T.pack . show <$> QC.chooseInt (1, 12)
  , QC.elements (map T.pack ["Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"])
  ]

genNamedMonthText :: QC.Gen T.Text
genNamedMonthText =
  QC.elements (map T.pack ["JAN","feb","Mar","apr","may","JUN","jul","AuG","sep","OCT","nov","dec"])

invalidMonth :: QC.Gen Int
invalidMonth = QC.oneof [QC.chooseInt (-12, 0), QC.chooseInt (13, 36)]

-- Helpers

parseTokenUnsafe :: T.Text -> Int
parseTokenUnsafe t =
  case parseAt (T.strip t) of
    Right m -> m
    Left err -> error ("unexpected parse failure: " <> err)
  where
    parseAt txt = do
      expr <- parseMonthsText txt
      case expr of
        AtMonth m -> Right m
        _         -> Left ("expected AtMonth for token " <> show txt)

isStepProgression :: Int -> Int -> [Int] -> Bool
isStepProgression _ _ [] = True
isStepProgression start step (x0 : rest) =
  x0 == start && all (== step) (consecutiveDiffs x0 rest)
  where
    consecutiveDiffs _ [] = []
    consecutiveDiffs prev (y : ys) = (y - prev) : consecutiveDiffs y ys