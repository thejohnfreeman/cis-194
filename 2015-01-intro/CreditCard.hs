import Test.HUnit
import Test.QuickCheck (quickCheck, (==>))

toDigits :: Integer -> [Integer]
toDigits = map (read . (:[])) . show

toDigitsRev :: Integer -> [Integer]
toDigitsRev n | n == 0 = []
              | n < 0  = error "cannot accept negative integers"
              | otherwise = (n `mod` 10) : toDigitsRev (n `div` 10)

everyOther :: (a -> a) -> [a] -> [a]
everyOther f (x:y:xs) = x : f y : everyOther f xs
everyOther _ xs = xs

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = everyOther (2*)

sumDigits :: [Integer] -> Integer
sumDigits = sum . concatMap toDigitsRev

luhn :: Integer -> Bool
luhn = (== 0) . (`mod` 10) . sumDigits . doubleEveryOther . toDigitsRev

main :: IO ()
main = do
  sequence_
    [ quickCheck canReassembleDigits
    , quickCheck isReverseDigits
    ]
  _ <- runTestTT $ test
    [ "doubleEveryOther" ~: doubleEveryOther [1,2,3,4] ~?= [1,4,3,8]
    , "sumDigits" ~: sumDigits [10, 5, 18, 4] ~?= 19
    , "ccGood" ~: assertBool "good" $ luhn 5594589764218858
    , "ccBad" ~: assertBool "bad" $ not $ luhn 1234567898765432
    ]
  return ()
    where
      canReassembleDigits n = n >= 0 ==> show n == concatMap show (toDigits n)
      isReverseDigits n = n > 0 ==> toDigits n == reverse (toDigitsRev n)

