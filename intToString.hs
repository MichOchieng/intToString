
numberToWord :: Int -> String
numberToWord n
  |(n > 9) && (n < 20)    = teens !! (n - 10)
  | n < 20                = shortNumber20 n
  | n < 100               = tensName !! ((n `div` 10)-1) `hyphen` numberToWord (n `mod` 10)
  |(n > 99) && (n < 1000) = hundredHyphen (shortNumber20 (n `div` 100)) `hyphen`  numberToWord (n `mod` 100)
  | otherwise             = "That's a lot of digits!"

tensName = ["","twenty","thirty","fourty","fifty","sixty","seventy","eighty","ninety"]

teens    = ["ten","eleven","twelve","thirteen","fourteen","fifteen","sixteen","seventeen","eighteen","nineteen"]

hyphen :: String -> String -> String
hyphen "" y   = y
hyphen x ""   = x
hyphen x y    = x ++ "-" ++ y

hundredHyphen ::  String -> String
hundredHyphen x =  x ++ "-" ++ "hundred"

shortNumber20 :: Int -> String
shortNumber20 n =
  head . drop (n) $ theWords where
    theWords = [ "", "one", "two", "three","four","five", "six", "seven", "eight","nine"]