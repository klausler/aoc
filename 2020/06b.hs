import qualified Data.Set as S
groups = map words . lines . pp
  where
    pp ('\n':x@('\n':_)) = '\n':pp x
    pp ('\n':x) = ' ':pp x
    pp (x:xs) = x:pp xs
    pp "" = ""
common = S.elems . foldr1 S.intersection . map S.fromList
doit = sum . map (length.common) . groups
main = fmap doit (readFile "in/06.txt") >>= print
