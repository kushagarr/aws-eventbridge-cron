{-# LANGUAGE OverloadedStrings #-}

module AWS.EventBridge.Minutes where

import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
import Data.Void

type Parser = Parsec Void Text

data MinutesExprT
  = AllMinutes
  | AtMinute Int
  | RangeMinute Int Int
  | StepMinute Int Int
  | UnionMinute MinutesExprT MinutesExprT
  deriving (Eq, Show)

isValidMinute :: Int -> Bool
isValidMinute m = 0 <= m && m <= 59

evaluateMinuteT :: MinutesExprT -> Either String [Int]
evaluateMinuteT mExpr = fmap S.toAscList (go mExpr >>= ensureBounds . S.fromList)
  where
    ensureBounds xs =
      if all isValidMinute xs
        then Right xs
        else Left "minute out of bounds: expected 0..59"
    go AllMinutes = Right [0..59]
    go (AtMinute m)
      | isValidMinute m = Right [m]
      | otherwise       = Left ("invalid minute: " ++ show m ++ " (expected 0..59)")
    go (RangeMinute a b)
      | isValidMinute a && isValidMinute b && a <= b = Right [a..b]
      | not (isValidMinute a) = Left ("invalid range start: " ++ show a ++ " (expected 0..59)")
      | not (isValidMinute b) = Left ("invalid range end: " ++ show b ++ " (expected 0..59)")
      | otherwise             = Left ("invalid range: start " ++ show a ++ " > end " ++ show b)
    go (StepMinute m s)
      | not (isValidMinute m) = Left ("invalid step start: " ++ show m ++ " (expected 0..59)")
      | s >= 1                = Right (takeWhile (<= 59) [m, m + s ..])
      | otherwise             = Left ("invalid step: " ++ show s ++ " (expected >= 1)")
    go (UnionMinute a b) = (++) <$> go a <*> go b


parseMinutes :: Parser MinutesExprT
parseMinutes = choice
  [ try parseUnionMinute
  , try parseStepMinute
  , try parseRangeMinute
  , try parseAtMinute
  , parseAllMinute
  ]

parseAllMinute :: Parser MinutesExprT
parseAllMinute = char '*' >> pure  AllMinutes

parseAtMinute :: Parser MinutesExprT
parseAtMinute = AtMinute <$> decimal

parseRangeMinute :: Parser MinutesExprT
parseRangeMinute = do
  start <- decimal
  _ <- char '-'
  RangeMinute start <$> decimal

parseStepMinute :: Parser MinutesExprT
parseStepMinute = do
  start <- decimal
  _ <- char '/'
  StepMinute start <$> decimal

parseUnionMinute :: Parser MinutesExprT
parseUnionMinute = do
  s <- choice [try parseStepMinute, try parseRangeMinute, try parseAtMinute, parseAllMinute]
  _ <- char ','
  e <- choice [try parseUnionMinute, try parseStepMinute, try parseRangeMinute, try parseAtMinute, parseAllMinute]
  eof
  pure $ UnionMinute s e

parseMinutesText :: Text -> Either String MinutesExprT
parseMinutesText input =
  case parse parseMinutes "minutes" (T.strip input) of
    Left err -> Left (errorBundlePretty err)
    Right expr -> Right expr
