{-# LANGUAGE OverloadedStrings #-}

module AWS.EventBridge.OneTime where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)
import Data.Time.Format (defaultTimeLocale, parseTimeM)
import Data.Time.LocalTime (LocalTime, localTimeToUTC, utc)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char (char, space, string)

type Parser = Parsec Void Text

newtype OneTimeExprT
  = OneTime UTCTime
  deriving (Eq, Show)

evaluateOneTimeT :: OneTimeExprT -> UTCTime
evaluateOneTimeT (OneTime t) = t

parseOneTimeText :: Text -> Either String OneTimeExprT
parseOneTimeText input =
  case parse parseOneTime "one-time" (T.strip input) of
    Left err -> Left (errorBundlePretty err)
    Right expr -> Right expr

parseOneTime :: Parser OneTimeExprT
parseOneTime = do
  _ <- string "at("
  _ <- space
  raw <- takeWhile1P (Just "timestamp") (/= ')')
  ts <- case timestampFromText (T.strip raw) of
    Just ts' -> pure ts'
    Nothing -> fail "invalid one-time timestamp"
  _ <- space
  _ <- char ')'
  eof
  pure (OneTime ts)

timestampFromText :: Text -> Maybe UTCTime
timestampFromText = parseLocal
  where
    parseLocal t = do
      local <- parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S" (T.unpack t) :: Maybe LocalTime
      pure (localTimeToUTC utc local)