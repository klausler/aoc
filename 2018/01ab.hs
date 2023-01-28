import qualified Data.Set as S
findDup set (x:xs)
  | S.member x set = x
  | otherwise = findDup (S.insert x set) xs
main = do
  input <- ((read <$>) . lines . filter (/='+')) <$> readFile "in/01.txt"
  print $ sum input -- part A
  print $ findDup S.empty $ scanl1 (+) $ cycle input -- part B
