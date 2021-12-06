import Control.Applicative((<$>))
codeVal xs  = cv 0 xs
  where
    cv n "" = n
    cv n (x:xs') = cv (2*n + val x) xs'
    val 'B' = 1
    val 'R' = 1
    val _ = 0
doit = foldr1 max . map codeVal . lines
main = doit <$> readFile "in/05.txt" >>= print
