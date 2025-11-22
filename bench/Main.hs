{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import AWS.EventBridge.Cron (CronExprT, nextRunTimes, parseCronText)
import AWS.EventBridge.Schedule
  ( Schedule
  , nextRunTimesLocal
  , scheduleFromText
  )
import Criterion.Main
import Data.Time
  ( LocalTime(..)
  , UTCTime(..)
  , fromGregorian
  , secondsToDiffTime
  , TimeOfDay(..)
  )
import Data.Time.Zones.All (TZLabel(..))

main :: IO ()
main = defaultMain
  [ bench "cron sparse 256" $ nf (runCron cronSparseExpr) 256
  , bench "cron dense 1024" $ nf (runCron cronDenseExpr) 1024
  , bench "cron dense 4096" $ nf (runCron cronDenseExpr) 4096
  , bench "rate sequential 2048" $ nf (runCron rateExpr) 2048
  , bench "schedule local 512" $ nf (runSchedule scheduleNY) 512
  ]

runCron :: CronExprT -> Int -> [UTCTime]
runCron expr limit =
  forceRight "nextRunTimes" $ nextRunTimes cronBase limit expr

runSchedule :: Schedule -> Int -> [LocalTime]
runSchedule sched limit =
  forceRight "nextRunTimesLocal" $ nextRunTimesLocal scheduleBase limit sched

cronBase :: UTCTime
cronBase = UTCTime (fromGregorian 2025 1 1) (secondsToDiffTime (5 * 3600))

scheduleBase :: LocalTime
scheduleBase = LocalTime (fromGregorian 2025 11 1) (TimeOfDay 8 30 0)

cronSparseExpr :: CronExprT
cronSparseExpr =
  forceRight "parseCronText cronSparse" $ parseCronText "cron(0 9 ? NOV SUN 2025)"

cronDenseExpr :: CronExprT
cronDenseExpr =
  forceRight "parseCronText cronDense" $
    parseCronText "cron(0/5 0-12 ? JAN,MAR,SEP MON-FRI 2025-2027)"

rateExpr :: CronExprT
rateExpr = forceRight "parseCronText rate" $ parseCronText "rate(5 minutes)"

scheduleNY :: Schedule
scheduleNY =
  forceRight "scheduleFromText" $
    scheduleFromText America__New_York "cron(0/15 0-23 * * ? *)"

forceRight :: String -> Either String a -> a
forceRight label = either (error . mkMsg) id
  where
    mkMsg err = "benchmark setup failed in " <> label <> ": " <> err
