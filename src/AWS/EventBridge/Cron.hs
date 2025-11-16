{-# LANGUAGE OverloadedStrings  #-}

module AWS.EventBridge.Cron where
import AWS.EventBridge.Minutes ( MinutesExprT, parseMinutesText, evaluateMinuteT )
import AWS.EventBridge.Hours ( HoursExprT, parseHoursText, evaluateHourT )
import AWS.EventBridge.DayOfMonth ( DayOfMonthExprT(..), parseDayOfMonthText, evaluateDayOfMonthT )
import AWS.EventBridge.Months ( MonthsExprT, parseMonthsText, evaluateMonthT )
import AWS.EventBridge.DayOfWeek ( DayOfWeekExprT(..), parseDayOfWeekText, evaluateDayOfWeekT )
import AWS.EventBridge.Years ( YearsExprT, parseYearsText, evaluateYearT )
import AWS.EventBridge.Rate ( RateExprT, parseRate, evaluateRateT )
import AWS.EventBridge.OneTime ( OneTimeExprT (..), parseOneTime, evaluateOneTimeT )
import Text.Megaparsec ( Parsec, try, (<|>), parse, errorBundlePretty, takeWhileP )
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec.Char
import qualified Data.Text as T
import Data.Time (UTCTime(..), Day, addUTCTime, utctDay)
import Data.Time.Calendar (fromGregorian, toGregorian)
import Data.Time.LocalTime (TimeOfDay(..), timeOfDayToTime)


data CronExprT
  = CronExpr
      { minutes    :: MinutesExprT
      , hours      :: HoursExprT
      , dayOfMonth :: DayOfMonthExprT
      , month      :: MonthsExprT
      , dayOfWeek  :: DayOfWeekExprT
      , year       :: YearsExprT
      }
  | RateExpr RateExprT
  | OneTimeExpr OneTimeExprT
  deriving (Eq, Show)

type Parser = Parsec Void Text

parseCronText :: Text -> Either String CronExprT
parseCronText input =
  case parse parseCron "cron" (T.strip input) of
    Left err  -> Left (errorBundlePretty err)
    Right val -> Right val

parseCron :: Parser CronExprT
parseCron = try parseRateExpr <|> try parseOneTimeExpr <|> parseCronExpr

parseRateExpr :: Parser CronExprT
parseRateExpr = RateExpr <$> parseRate

parseOneTimeExpr :: Parser CronExprT
parseOneTimeExpr = OneTimeExpr <$> parseOneTime

parseCronExpr :: Parser CronExprT
parseCronExpr = do
    _ <- string "cron("
    body <- takeWhileP (Just "cron body") (/= ')')
    _ <- char ')'
    case T.words (T.strip body) of
      [minutesText, hoursText, domText, monthText, dowText, yearText] -> do
        minExpr <- liftEither (parseMinutesText minutesText)
        hourExpr <- liftEither (parseHoursText hoursText)
        domExpr <- liftEither (parseDayOfMonthText domText)
        monthExpr <- liftEither (parseMonthsText monthText)
        dowExpr <- liftEither (parseDayOfWeekText dowText)
        yearExpr <- liftEither (parseYearsText yearText)
        pure (CronExpr minExpr hourExpr domExpr monthExpr dowExpr yearExpr)
      _ -> fail "cron expression must contain six space-delimited fields"
  where
    liftEither :: Either String a -> Parser a
    liftEither = either fail pure



nextRunTimes :: CronExprT -> UTCTime -> Int -> Either String [UTCTime]
nextRunTimes expr base limit =
  case expr of
    RateExpr r -> futureRateTimes r base limit
    OneTimeExpr o  -> futureOneTime o base limit
    CronExpr m h dom mon dow yr -> futureCronTimes m h dom mon dow yr base limit


futureOneTime :: OneTimeExprT -> UTCTime -> Int -> Either String [UTCTime]
futureOneTime expr base limit
  | t >= base && limit > 0 = Right [t]
  | otherwise              = Right []
  where
    t = evaluateOneTimeT expr


futureRateTimes :: RateExprT -> UTCTime -> Int -> Either String [UTCTime]
futureRateTimes expr base limit = case evaluateRateT expr of
  Left err -> Left err
  Right delta -> Right $ take limit $ iterate (addUTCTime delta) base

futureCronTimes :: MinutesExprT -> HoursExprT -> DayOfMonthExprT -> MonthsExprT -> DayOfWeekExprT -> YearsExprT -> UTCTime -> Int -> Either String [UTCTime]
futureCronTimes minExpr hourExpr domExpr monExpr dowExpr yrExpr base limit
  | domIsQuestion == dowIsQuestion = Left "day-of-month and day-of-week fields must use '?' in exactly one position"
  | otherwise = do
      minutes <- evaluateMinuteT minExpr
      hours <- evaluateHourT hourExpr
      yearCandidates <- fmap (map fromIntegral) (evaluateYearT yrExpr)
      monthCandidates <- evaluateMonthT monExpr
      let target = max 0 limit
          baseDay = utctDay base
          (baseYear, baseMonth, baseDom) = toGregorian baseDay

          collectYears :: [UTCTime] -> Int -> [Integer] -> Either String ([UTCTime], Int)
          collectYears acc count [] = Right (acc, count)
          collectYears acc count _ | count >= target = Right (acc, count)
          collectYears acc count (y:ys)
            | y < baseYear = collectYears acc count ys
            | otherwise = do
                (acc', count') <- collectMonths acc count y monthCandidates
                collectYears acc' count' ys

          collectMonths :: [UTCTime] -> Int -> Integer -> [Int] -> Either String ([UTCTime], Int)
          collectMonths acc count _ [] = Right (acc, count)
          collectMonths acc count _ _ | count >= target = Right (acc, count)
          collectMonths acc count year (m:ms)
            | year == baseYear && m < baseMonth = collectMonths acc count year ms
            | otherwise = do
                days <- daysFor year m
                (acc', count') <- collectDays acc count year m days
                collectMonths acc' count' year ms

          collectDays :: [UTCTime] -> Int -> Integer -> Int -> [Int] -> Either String ([UTCTime], Int)
          collectDays acc count _ _ [] = Right (acc, count)
          collectDays acc count _ _ _ | count >= target = Right (acc, count)
          collectDays acc count year month (d:ds)
            | year == baseYear && month == baseMonth && d < baseDom = collectDays acc count year month ds
            | otherwise =
                let dayDate = fromGregorian year month d
                    remaining = target - count
                    dayTimes = buildDayTimes dayDate
                    filteredTimes = case compare dayDate baseDay of
                      LT -> []
                      EQ -> dropWhile (< base) dayTimes
                      GT -> dayTimes
                    selected = take remaining filteredTimes
                    acc' = acc ++ selected
                    count' = count + length selected
                 in if count' >= target
                      then Right (acc', count')
                      else collectDays acc' count' year month ds

          buildDayTimes :: Day -> [UTCTime]
          buildDayTimes dayDate =
            [ UTCTime dayDate (timeOfDayToTime (TimeOfDay hour minute 0))
            | hour <- hours
            , minute <- minutes
            ]

          daysFor :: Integer -> Int -> Either String [Int]
          daysFor year month = do
            domDays <- evaluateDayOfMonthT year month domExpr
            dowDays <- evaluateDayOfWeekT year month dowExpr
            pure $ if domIsQuestion then dowDays else domDays

      (results, _) <- collectYears [] 0 yearCandidates
      pure results
  where
    domIsQuestion = isDomQuestion domExpr
    dowIsQuestion = isDowQuestion dowExpr


isDomQuestion :: DayOfMonthExprT -> Bool
isDomQuestion DomAny = True
isDomQuestion _ = False

isDowQuestion :: DayOfWeekExprT -> Bool
isDowQuestion DowAny = True
isDowQuestion _ = False