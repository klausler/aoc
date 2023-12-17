import Data.Char(isLetter)
import Data.List(groupBy,sortBy)
import qualified Data.Map as M
import qualified Data.Set as S
((.*.) `on` f) x y = f x .*. f y
func cv ch = (17 * (cv + fromEnum ch)) `rem` 256
hashA = foldl func 0
hashB = foldl func 0 . takeWhile isLetter
revScan _ order [] = order
revScan m order (w:ws)
  | ('=':[_]) <- rest, Just 0 <- M.lookup name m = revScan m order ws
  | ('=':[_]) <- rest, Just n <- M.lookup name m = revScan m ((name,n):order) ws
  | ('=':[vch]) <- rest = let n = read [vch] in revScan (M.insert name n m) ((name,n):order) ws
  | otherwise = revScan (M.insert name 0 m) order ws
  where (name,rest) = span isLetter w
keepFirst _ [] = []
keepFirst set ((name,n):rest)
  | name `S.member` set = keepFirst set rest
  | otherwise = n : keepFirst (S.insert name set) rest
main = do
  input <- map (\ch -> if ch == ',' then ' ' else ch) <$> readFile "in/15.txt"
  let wds = words input
  print $ sum $ fmap hashA wds -- part A
  let boxed = groupBy ((==) `on` hashB) $ sortBy (compare `on` hashB) wds
  print $ sum [ (1 + (hashB $ head box)) * pos * lensVal | box <- boxed,
                  (pos,lensVal) <- zip [1..] $ keepFirst S.empty $
                                   revScan M.empty [] $ reverse box ] -- part B
