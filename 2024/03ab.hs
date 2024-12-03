isDigit ch = ch >= '0' && ch <= '9'
getInt str
  | length digits >= 1, length digits <= 3 = Just (read digits, rest)
  | otherwise = Nothing
  where (digits,rest) = span isDigit str
begins "" str = Just str
begins (x:xs) (y:ys) | x == y = begins xs ys
begins _ _ = Nothing
getMul str = do
  str1 <- "mul(" `begins` str
  (x, str2) <- getInt str1
  str3 <- "," `begins` str2
  (y, str4) <- getInt str3
  str5 <- ")" `begins` str4
  Just (x * y, str5)
process n "" = n
process n str
  | Just (prod, str') <- getMul str = process (n+prod) str'
  | otherwise = process n $ tail str
skipPastDo "" = ""
skipPastDo str
  | Just str' <- "do()" `begins` str = str'
  | otherwise = skipPastDo $ tail str
disableDonts "" = ""
disableDonts (str@(ch:rest))
  | Just str' <- "don't()" `begins` str = disableDonts $ skipPastDo str'
  | otherwise = ch : (disableDonts $ tail str)
part2 _ n "" = n
main = do
  input <- readFile "in/03.txt"
  print $ process 0 input -- part 1
  print $ process 0 $ disableDonts input -- part 2
