import Data.Char(digitToInt, isDigit)
import Data.List(isPrefixOf)
english = zip ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"] [1..]
toDigits "" = []
toDigits (d:rest) | isDigit d = digitToInt d : toDigits rest
toDigits s | not $ null dv = dv ++ toDigits (tail s)
  where dv = [ j | (e,j) <- english, isPrefixOf e s ]
toDigits (_:rest) = toDigits rest
main = do
  lns <- lines <$> readFile "in/01.txt"
  let ds = toDigits <$> lns
      azs = zip (head <$> ds) (last <$> ds)
  print $ sum [ 10*ad + zd | (ad,zd) <- azs ]
