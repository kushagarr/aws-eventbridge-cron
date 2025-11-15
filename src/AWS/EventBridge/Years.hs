{-# LANGUAGE OverloadedStrings #-}

module AWS.EventBridge.Years where

import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec (Parsec, choice, eof, errorBundlePretty, parse, try)
import Text.Megaparsec.Char (char)
import Text.Megaparsec.Char.Lexer (decimal)

type Parser = Parsec Void Text

data YearsExprT
  = AllYears
  | AtYear Int
  | RangeYear Int Int
  | StepYear Int Int
  | UnionYear YearsExprT YearsExprT
  deriving (Eq, Show)

-- AWS EventBridge supports calendar years 1970..2199 inclusive.
isValidYear :: Int -> Bool
isValidYear y = 1970 <= y && y <= 2199

evaluateYearT :: YearsExprT -> Either String [Int]
evaluateYearT expr = fmap S.toAscList (go expr >>= ensureBounds . S.fromList)
  where
    ensureBounds xs =
      if all isValidYear xs
        then Right xs
        else Left "year out of bounds: expected 1970..2199"
    go AllYears = Right [1970..2199]
    go (AtYear y)
      | isValidYear y = Right [y]
      | otherwise     = Left ("invalid year: " ++ show y ++ " (expected 1970..2199)")
    go (RangeYear a b)
      | isValidYear a && isValidYear b && a <= b = Right [a..b]
      | not (isValidYear a) = Left ("invalid range start: " ++ show a ++ " (expected 1970..2199)")
      | not (isValidYear b) = Left ("invalid range end: " ++ show b ++ " (expected 1970..2199)")
      | otherwise           = Left ("invalid range: start " ++ show a ++ " > end " ++ show b)
    go (StepYear start step)
      | not (isValidYear start) = Left ("invalid step start: " ++ show start ++ " (expected 1970..2199)")
      | step >= 1 = Right (takeWhile (<= 2199) [start, start + step ..])
      | otherwise = Left ("invalid step: " ++ show step ++ " (expected >= 1)")
    go (UnionYear a b) = (++) <$> go a <*> go b

parseYears :: Parser YearsExprT
parseYears = choice
  [ try parseUnionYear
  , try parseStepYear
  , try parseRangeYear
  , try parseAtYear
  , parseAllYear
  ]

parseAllYear :: Parser YearsExprT
parseAllYear = char '*' >> pure AllYears

parseAtYear :: Parser YearsExprT
parseAtYear = AtYear <$> decimal

parseRangeYear :: Parser YearsExprT
parseRangeYear = do
  start <- decimal
  _ <- char '-'
  RangeYear start <$> decimal

parseStepYear :: Parser YearsExprT
parseStepYear = do
  start <- decimal
  _ <- char '/'
  StepYear start <$> decimal

parseUnionYear :: Parser YearsExprT
parseUnionYear = do
  s <- choice [try parseStepYear, try parseRangeYear, try parseAtYear, parseAllYear]
  _ <- char ','
  e <- choice [try parseUnionYear, try parseStepYear, try parseRangeYear, try parseAtYear, parseAllYear]
  eof
  pure (UnionYear s e)

parseYearsText :: Text -> Either String YearsExprT
parseYearsText input =
  case parse parseYears "years" (T.strip input) of
    Left err -> Left (errorBundlePretty err)
    Right expr -> Right expr