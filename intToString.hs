module Numbers
where

numberToWord :: Integer -> String
numberToWord n
  | n < 20      = shortNumber20 n
  | n < 100     = tensName (n `div` 10) `hyphen`
                   numberToWord (n `mod` 10)
  | otherwise   = "frog"

tensName _ = "thirtyIsh"

hyphen :: String -> String -> String
hyphen "" y =  y
hyphen x "" = x
hyphen x y = x ++ "-" ++ y


shortNumber20 :: Integer -> String
shortNumber20 n =
  head . drop (fromInteger n) $  theWords where
    theWords = [ "", "one", "two", "three","four","five", "six", "seven", "eight","nine" ]