{-# LANGUAGE OverloadedStrings #-}

module AWS.EventBridge.Months where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
import Data.Void

type Parser = Parsec Void Text

data MonthsExprT
  = AllMonths
  | AtMonth Int
  | RangeMonth Int Int
  | StepMonth Int Int
  | UnionMonth MonthsExprT MonthsExprT
  deriving (Eq, Show)

isValidMonth :: Int -> Bool
isValidMonth m = 1 <= m && m <= 12

evaluateMonthT :: MonthsExprT -> Either String [Int]
evaluateMonthT expr = fmap S.toAscList (go expr >>= ensureBounds . S.fromList)
  where
    ensureBounds xs =
      if all isValidMonth xs
        then Right xs
        else Left "month out of bounds: expected 1..12"
    go AllMonths = Right [1..12]
    go (AtMonth m)
      | isValidMonth m = Right [m]
      | otherwise      = Left ("invalid month: " ++ show m ++ " (expected 1..12)")
    go (RangeMonth a b)
      | isValidMonth a && isValidMonth b && a <= b = Right [a..b]
      | not (isValidMonth a) = Left ("invalid range start: " ++ show a ++ " (expected 1..12)")
      | not (isValidMonth b) = Left ("invalid range end: " ++ show b ++ " (expected 1..12)")
      | otherwise            = Left ("invalid range: start " ++ show a ++ " > end " ++ show b)
    go (StepMonth m step)
      | not (isValidMonth m) = Left ("invalid step start: " ++ show m ++ " (expected 1..12)")
      | step >= 1            = Right (takeWhile (<= 12) [m, m + step ..])
      | otherwise            = Left ("invalid step: " ++ show step ++ " (expected >= 1)")
    go (UnionMonth a b) = (++) <$> go a <*> go b

monthNames :: M.Map Text Int
monthNames = M.fromList
  [ ("JAN", 1), ("FEB", 2), ("MAR", 3), ("APR", 4)
  , ("MAY", 5), ("JUN", 6), ("JUL", 7), ("AUG", 8)
  , ("SEP", 9), ("OCT", 10), ("NOV", 11), ("DEC", 12)
  ]

parseMonths :: Parser MonthsExprT
parseMonths = choice
  [ try parseUnionMonth
  , try parseStepMonth
  , try parseRangeMonth
  , try parseAtMonth
  , parseAllMonth
  ]

parseAllMonth :: Parser MonthsExprT
parseAllMonth = char '*' >> pure AllMonths

parseAtMonth :: Parser MonthsExprT
parseAtMonth = AtMonth <$> monthToken

parseRangeMonth :: Parser MonthsExprT
parseRangeMonth = do
  start <- monthToken
  _ <- char '-'
  RangeMonth start <$> monthToken

parseStepMonth :: Parser MonthsExprT
parseStepMonth = do
  start <- monthToken
  _ <- char '/'
  StepMonth start <$> decimal

parseUnionMonth :: Parser MonthsExprT
parseUnionMonth = do
  s <- choice [try parseStepMonth, try parseRangeMonth, try parseAtMonth, parseAllMonth]
  _ <- char ','
  e <- choice [try parseUnionMonth, try parseStepMonth, try parseRangeMonth, try parseAtMonth, parseAllMonth]
  eof
  pure (UnionMonth s e)

monthToken :: Parser Int
monthToken = try named <|> decimal
  where
    named = do
      txt <- some letterChar
      case M.lookup (T.toUpper (T.pack txt)) monthNames of
        Just n  -> pure n
        Nothing -> fail ("unknown month name: " ++ txt)

parseMonthsText :: Text -> Either String MonthsExprT
parseMonthsText input =
  case parse parseMonths "months" (T.strip input) of
    Left err -> Left (errorBundlePretty err)
    Right expr -> Right expr