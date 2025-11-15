module TestSupport
  ( assertLeft
  , expectEvalWith
  , expectParseWith
  , expectEvalEquals
  , strictlyAscending
  , withinBoundsInt
  ) where

import Test.Tasty.HUnit (Assertion, assertFailure)

assertLeft :: Either String a -> Assertion
assertLeft e = case e of
  Left _  -> pure ()
  Right _ -> assertFailure "expected Left"

expectParseWith :: String -> (input -> Either String a) -> input -> IO a
expectParseWith label parser input =
  case parser input of
    Left err -> assertFailure (label <> " parse failed: " <> err)
    Right value -> pure value

expectEvalWith :: String -> (expr -> Either String result) -> expr -> IO result
expectEvalWith label evaluator expr =
  case evaluator expr of
    Left err -> assertFailure (label <> " evaluate failed: " <> err)
    Right value -> pure value

expectEvalEquals :: (Eq a, Show a) => [a] -> Either String [a] -> Assertion
expectEvalEquals expected evalResult =
  case evalResult of
    Left err   -> assertFailure err
    Right xs ->
      if xs == expected
        then pure ()
        else assertFailure ("expected " <> show expected <> ", got " <> show xs)

withinBoundsInt :: Int -> Int -> [Int] -> Bool
withinBoundsInt lo hi = all (\x -> x >= lo && x <= hi)

strictlyAscending :: Ord a => [a] -> Bool
strictlyAscending [] = True
strictlyAscending [_] = True
strictlyAscending (x : y : rest) = x < y && strictlyAscending (y : rest)
