{-# LANGUAGE OverloadedStrings #-}

module AWS.EventBridge.DayOfWeek where

import Control.Applicative ((<|>), some)
import Control.Monad (when)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Char (toUpper)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Calendar (fromGregorian, gregorianMonthLength)
import Data.Time.Calendar.WeekDate (toWeekDate)
import Data.Void (Void)
import Text.Megaparsec (Parsec, choice, eof, errorBundlePretty, parse, sepBy1, try)
import Text.Megaparsec.Char (char, letterChar)
import Text.Megaparsec.Char.Lexer (decimal)

type Parser = Parsec Void Text

data DayOfWeekExprT
  = DowAny
  | DowAll
  | DowAt Int
  | DowRange Int Int
  | DowNth Int Int
  | DowUnion DayOfWeekExprT DayOfWeekExprT
  deriving (Eq, Show)

isValidDayOfWeek :: Int -> Bool
isValidDayOfWeek d = 1 <= d && d <= 7

evaluateDayOfWeekT :: Integer -> Int -> DayOfWeekExprT -> Either String [Int]
evaluateDayOfWeekT year month expr = fmap S.toAscList (S.fromList <$> go expr)
  where
    dim = gregorianMonthLength year month
    go DowAny = Right []
    go DowAll = Right [1 .. dim]
    go (DowAt day)
      | isValidDayOfWeek day = Right (matchingDays day)
      | otherwise = Left ("invalid day-of-week: " ++ show day ++ " (expected 1..7 or SUN-SAT)")
    go (DowRange start end)
      | not (isValidDayOfWeek start) = Left ("invalid range start: " ++ show start ++ " (expected 1..7)")
      | not (isValidDayOfWeek end) = Left ("invalid range end: " ++ show end ++ " (expected 1..7)")
      | start <= end = Right (concatMap matchingDays [start .. end])
      | otherwise = Left ("invalid range: start " ++ show start ++ " > end " ++ show end)
    go (DowNth day nth)
      | not (isValidDayOfWeek day) = Left ("invalid day-of-week: " ++ show day ++ " (expected 1..7 or SUN-SAT)")
      | nth < 1 || nth > 5 = Left ("invalid # occurrence: " ++ show nth ++ " (expected 1..5)")
      | otherwise =
          let occurrences = matchingDays day
          in case drop (nth - 1) occurrences of
               (d : _) -> Right [d]
               [] -> Right []
    go (DowUnion a b) = (++) <$> go a <*> go b

    -- | Enumerate the day-of-month values that fall on the requested weekday.
    -- Example: in April 2025 (month=4) Mondays (target=2) occur on
    -- days [7,14,21,28].
    matchingDays target =
      [ dayOfMonth
      | dayOfMonth <- [1 .. dim]
      , awsDayOfWeek year month dayOfMonth == target
      ]

parseDayOfWeek :: Parser DayOfWeekExprT
parseDayOfWeek = do
  terms <- dowTerm `sepBy1` char ','
  eof
  when (any isAny terms && length terms > 1) $
    fail "? cannot be combined with other day-of-week terms"
  when (any isNth terms && length terms > 1) $
    fail "# expressions cannot be combined with other day-of-week terms"
  pure (foldUnion terms)

parseDayOfWeekText :: Text -> Either String DayOfWeekExprT
parseDayOfWeekText input =
  case parse parseDayOfWeek "day-of-week" (T.strip input) of
    Left err -> Left (errorBundlePretty err)
    Right expr -> Right expr

dowTerm :: Parser DayOfWeekExprT
dowTerm = choice
  [ DowAny <$ char '?'
  , DowAll <$ char '*'
  , try parseNthTerm
  , try parseRangeTerm
  , DowAt <$> dowValue
  ]

parseRangeTerm :: Parser DayOfWeekExprT
parseRangeTerm = do
  start <- dowValue
  _ <- char '-'
  DowRange start <$> dowValue

parseNthTerm :: Parser DayOfWeekExprT
parseNthTerm = do
  day <- dowValue
  _ <- char '#'
  DowNth day <$> decimal

dowValue :: Parser Int
dowValue = try named <|> decimal
  where
    named = do
      txt <- someLetters
      case M.lookup (T.pack txt) dayOfWeekNames of
        Just value -> pure value
        Nothing -> fail ("unknown day-of-week name: " ++ txt)

-- | Parse one or more alphabetic characters and normalise to uppercase.
-- Example: parsing "tHu" yields "THU" which matches tokens like THU/THUR.
someLetters :: Parser String
someLetters = fmap (map toUpper) (some letterChar)

dayOfWeekNames :: M.Map Text Int
dayOfWeekNames = M.fromList
  [ ("SUN", 1), ("MON", 2), ("TUE", 3), ("TUES", 3)
  , ("WED", 4), ("THU", 5), ("THUR", 5), ("THURS", 5)
  , ("FRI", 6), ("SAT", 7)
  ]

foldUnion :: [DayOfWeekExprT] -> DayOfWeekExprT
-- | Combine a non-empty list of terms into a left-associated union.
-- Example: [DowAt 1, DowRange 2 3, DowNth 5 1] becomes
-- DowUnion (DowUnion (DowAt 1) (DowRange 2 3)) (DowNth 5 1).
foldUnion [] = DowAny
foldUnion (x : xs) = foldl DowUnion x xs

isAny :: DayOfWeekExprT -> Bool
isAny DowAny = True
isAny _ = False

isNth :: DayOfWeekExprT -> Bool
isNth (DowNth _ _) = True
isNth _ = False

-- | Convert a calendar date into the AWS EventBridge weekday index.
-- Example: 2025-04-07 (a Monday) yields 2; 2025-04-13 (a Sunday) yields 1.
awsDayOfWeek :: Integer -> Int -> Int -> Int
awsDayOfWeek year month dayOfMonth =
  let (_, _, iso) = toWeekDate (fromGregorian year month dayOfMonth)
  in (iso `mod` 7) + 1