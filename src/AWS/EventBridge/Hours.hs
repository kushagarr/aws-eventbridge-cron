{-# LANGUAGE OverloadedStrings #-}

module AWS.EventBridge.Hours where

import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
import Data.Void

type Parser = Parsec Void Text

data HoursExprT
  = AllHours
  | AtHour Int
  | RangeHour Int Int
  | StepHour Int Int
  | UnionHour HoursExprT HoursExprT
  deriving (Eq, Show)

isValidHour :: Int -> Bool
isValidHour h = 0 <= h && h <= 23

evaluateHourT :: HoursExprT -> Either String [Int]
evaluateHourT hExpr = fmap S.toAscList (go hExpr >>= ensureBounds . S.fromList)
  where
    ensureBounds xs =
      if all isValidHour xs
        then Right xs
        else Left "hour out of bounds: expected 0..23"
    go AllHours = Right [0..23]
    go (AtHour h)
      | isValidHour h = Right [h]
      | otherwise     = Left ("invalid hour: " ++ show h ++ " (expected 0..23)")
    go (RangeHour a b)
      | isValidHour a && isValidHour b && a <= b = Right [a..b]
      | not (isValidHour a) = Left ("invalid range start: " ++ show a ++ " (expected 0..23)")
      | not (isValidHour b) = Left ("invalid range end: " ++ show b ++ " (expected 0..23)")
      | otherwise             = Left ("invalid range: start " ++ show a ++ " > end " ++ show b)
    go (StepHour m s)
      | not (isValidHour m) = Left ("invalid step start: " ++ show m ++ " (expected 0..23)")
      | s >= 1               = Right (takeWhile (<= 23) [m, m + s ..])
      | otherwise            = Left ("invalid step: " ++ show s ++ " (expected >= 1)")
    go (UnionHour a b) = (++) <$> go a <*> go b


parseHours :: Parser HoursExprT
parseHours = choice
  [ try parseUnionHour
  , try parseStepHour
  , try parseRangeHour
  , try parseAtHour
  , parseAllHour
  ]

parseAllHour :: Parser HoursExprT
parseAllHour = char '*' >> pure  AllHours

parseAtHour :: Parser HoursExprT
parseAtHour = AtHour <$> decimal

parseRangeHour :: Parser HoursExprT
parseRangeHour = do
  start <- decimal
  _ <- char '-'
  RangeHour start <$> decimal

parseStepHour :: Parser HoursExprT
parseStepHour = do
  start <- decimal
  _ <- char '/'
  StepHour start <$> decimal

parseUnionHour :: Parser HoursExprT
parseUnionHour = do
  s <- choice [try parseStepHour, try parseRangeHour, try parseAtHour, parseAllHour]
  _ <- char ','
  e <- choice [try parseUnionHour, try parseStepHour, try parseRangeHour, try parseAtHour, parseAllHour]
  eof
  pure $ UnionHour s e

parseHoursText :: Text -> Either String HoursExprT
parseHoursText input =
  case parse parseHours "hours" (T.strip input) of
    Left err -> Left (errorBundlePretty err)
    Right expr -> Right expr
