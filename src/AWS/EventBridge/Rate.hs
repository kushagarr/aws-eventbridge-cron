{-# LANGUAGE OverloadedStrings #-}

module AWS.EventBridge.Rate where
import Text.Megaparsec (Parsec, (<|>), try, parse, errorBundlePretty, optional)
import Data.Void (Void)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (decimal)
import Data.Time (NominalDiffTime, secondsToNominalDiffTime)

data RateExprT
  = RateMinutes Int
  | RateHours Int
  | RateDays Int
  deriving (Eq, Show)


type Parser = Parsec Void Text

evaluateRateT :: RateExprT -> Either String NominalDiffTime
evaluateRateT (RateMinutes m)
  | m >= 1 && m <= 31536000  = Right (secondsToNominalDiffTime (fromIntegral (m * 60)))
  | otherwise = Left ("invalid rate minutes: " ++ show m ++ " (expected 1..31536000)")
evaluateRateT (RateHours h)
  | h >= 1 && h <= 8760 = Right (secondsToNominalDiffTime (fromIntegral (h * 3600)))
  | otherwise = Left ("invalid rate hours: " ++ show h ++ " (expected 1..8760)")
evaluateRateT (RateDays d)
  | d >= 1 && d <= 365 = Right (secondsToNominalDiffTime (fromIntegral (d * 86400)))
  | otherwise = Left ("invalid rate days: " ++ show d ++ " (expected 1..365)")

parseRate :: Parser RateExprT
parseRate = try parseRateMinutes <|> try parseRateHours <|> parseRateDays

parseRateText :: Text -> Either String RateExprT
parseRateText input =
  case parse parseRate "rate" (T.strip input) of
    Left err  -> Left (errorBundlePretty err)
    Right val -> Right val

parseRateMinutes :: Parser RateExprT
parseRateMinutes = do
  _ <- string "rate("
  m <- decimal
  _ <- space1
  _ <- string "minute"
  _ <- optional (char 's')
  _ <- char ')'
  return (RateMinutes m)

parseRateHours :: Parser RateExprT
parseRateHours = do
  _ <- string "rate("
  h <- decimal
  _ <- space1
  _ <- string "hour"
  _ <- optional (char 's')
  _ <- char ')'
  return (RateHours h)

parseRateDays :: Parser RateExprT
parseRateDays = do
  _ <- string "rate("
  d <- decimal
  _ <- space1
  _ <- string "day"
  _ <- optional (char 's')
  _ <- char ')'
  return (RateDays d)