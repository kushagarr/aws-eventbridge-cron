{-# LANGUAGE OverloadedStrings #-}

module AWS.EventBridge.DayOfMonth where

import Control.Monad (unless)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Calendar (Day, fromGregorian, gregorianMonthLength)
import Data.Time.Calendar.WeekDate (toWeekDate)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

type Parser = Parsec Void Text

data DayOfMonthExprT
  = DomAny
  | DomAll
  | DomAt Int
  | DomRange Int Int
  | DomStep Int Int
  | DomUnion DayOfMonthExprT DayOfMonthExprT
  | DomLast
  | DomLastOffset Int
  | DomClosestWeekday Int
  | DomLastWeekday
  deriving (Eq, Show)

isValidDay :: Int -> Bool
isValidDay d = 1 <= d && d <= 31

evaluateDayOfMonthT :: Integer -> Int -> DayOfMonthExprT -> Either String [Int]
evaluateDayOfMonthT y m expr = fmap (S.toAscList . S.fromList . limit) (go expr)
  where
    dim = gregorianMonthLength y m
    limit = filter (\d -> d >= 1 && d <= dim)
    go DomAny = Right []
    go DomAll = Right [1 .. dim]
    go (DomAt d)
      | isValidDay d = Right [d]
      | otherwise    = Left ("invalid day: " ++ show d ++ " (expected 1..31)")
    go (DomRange a b)
      | not (isValidDay a) = Left ("invalid range start: " ++ show a ++ " (expected 1..31)")
      | not (isValidDay b) = Left ("invalid range end: " ++ show b ++ " (expected 1..31)")
      | a <= b             = Right [a .. b]
      | otherwise          = Left ("invalid range: start " ++ show a ++ " > end " ++ show b)
    go (DomStep s k)
      | not (isValidDay s) = Left ("invalid step start: " ++ show s ++ " (expected 1..31)")
      | k >= 1             = Right (takeWhile (<= 31) [s, s + k ..])
      | otherwise          = Left ("invalid step: " ++ show k ++ " (expected >= 1)")
    go (DomUnion a b) = (++) <$> go a <*> go b
    go DomLast = Right [dim]
    go (DomLastOffset off)
      | off < 1   = Left ("invalid L-n offset: " ++ show off ++ " (expected >= 1)")
      | off >= dim =
          Left ("invalid L-n offset: " ++ show off ++ " (expected < " ++ show dim ++ ")")
      | otherwise = Right [dim - off]
    go (DomClosestWeekday d)
      | isValidDay d = Right (maybe [] pure (closestWeekday y m d))
      | otherwise    = Left ("invalid nW day: " ++ show d ++ " (expected 1..31)")
    go DomLastWeekday = Right [lastWeekday y m]

parseDayOfMonth :: Parser DayOfMonthExprT
parseDayOfMonth = do
  terms <- domTerm `sepBy1` char ','
  eof
  case terms of
    [only] -> pure only
    xs -> do
      unless (all isNumeric xs) $ fail "comma-separated day expressions must be numeric"
      pure (foldUnion xs)

parseDayOfMonthText :: Text -> Either String DayOfMonthExprT
parseDayOfMonthText input =
  case parse parseDayOfMonth "day-of-month" (T.strip input) of
    Left err -> Left (errorBundlePretty err)
    Right expr -> Right expr

domTerm :: Parser DayOfMonthExprT
domTerm = choice
  [ DomAny <$ char '?'
  , try parseSpecialTerm
  , try parseStepTerm
  , try parseRangeTerm
  , try (DomAt <$> decimal)
  , DomAll <$ char '*'
  ]

parseRangeTerm :: Parser DayOfMonthExprT
parseRangeTerm = do
  start <- decimal
  _ <- char '-'
  DomRange start <$> decimal

parseStepTerm :: Parser DayOfMonthExprT
parseStepTerm = do
  start <- decimal
  _ <- char '/'
  DomStep start <$> decimal

parseSpecialTerm :: Parser DayOfMonthExprT
parseSpecialTerm = choice
  [ try (string "LW" >> pure DomLastWeekday)
  , try (do
        _ <- char 'L'
        _ <- char '-'
        DomLastOffset <$> decimal)
  , char 'L' >> pure DomLast
  , try (do
        d <- decimal
        _ <- char 'W'
        pure (DomClosestWeekday d))
  ]

isNumeric :: DayOfMonthExprT -> Bool
isNumeric DomAll         = True
isNumeric (DomAt _)      = True
isNumeric (DomRange _ _) = True
isNumeric (DomStep _ _)  = True
isNumeric _              = False

foldUnion :: [DayOfMonthExprT] -> DayOfMonthExprT
foldUnion (x:xs) = go x xs
  where
    go acc []       = acc
    go acc (y : ys) = go (DomUnion acc y) ys
foldUnion [] = DomAny

weekday :: Day -> Int
weekday dayValue = let (_, _, w) = toWeekDate dayValue in w

mkDay :: Integer -> Int -> Int -> Day
mkDay = fromGregorian

closestWeekday :: Integer -> Int -> Int -> Maybe Int
closestWeekday y m d =
  let dim = gregorianMonthLength y m
  in if d < 1 || d > dim
        then Nothing
        else
          case weekday (mkDay y m d) of
            1 -> Just d
            2 -> Just d
            3 -> Just d
            4 -> Just d
            5 -> Just d
            6 -> if d > 1 then Just (d - 1) else Just (d + 2)
            7 -> if d < dim then Just (d + 1) else Just (d - 2)
            _ -> Just d

lastWeekday :: Integer -> Int -> Int
lastWeekday y m =
  let dim = gregorianMonthLength y m
      w = weekday (mkDay y m dim)
  in case w of
      6 -> dim - 1
      7 -> dim - 2
      _ -> dim
