import Data.Char(digitToInt, intToDigit, isDigit)
import Data.List(isPrefixOf)
digits = [ ([intToDigit j], j) | j <- [0..9] ] ++
  zip ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"] [1..]
toDigits "" = []
toDigits s = [ j | (d,j) <- digits, isPrefixOf d s ] ++ toDigits (tail s)
main = do
  lns <- lines <$> readFile "in/01.txt"
  let ds = toDigits <$> lns
      azs = zip (head <$> ds) (last <$> ds)
  print $ sum [ 10*ad + zd | (ad,zd) <- azs ]
