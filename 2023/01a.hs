import Data.Char(digitToInt, isDigit)
main = do
  lns <- lines <$> readFile "in/01.txt"
  let ds = ((digitToInt <$>) . filter isDigit) <$> lns
      azs = zip (head <$> ds) (last <$> ds)
  print $ sum [ 10*ad + zd | (ad,zd) <- azs ]
