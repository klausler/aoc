import Data.List(transpose)
import qualified Data.Map as M
chFilter ch = if ch == '-' || ch >= '0' && ch <= '9' then ch else ' '
step :: [(Int,Int)] -> [(Int,Int)]
step xdxs = zip (zipWith (+) xs dxs') dxs'
  where (xs,dxs) = unzip xdxs
        ddx :: Int -> Int
        ddx x = sum $ (signum . (subtract x)) <$> xs
        dxs' = zipWith (+) dxs $ ddx <$> xs
findRepeat m n xdxs
  | Just start <- M.lookup xdxs m = (start,n)
  | otherwise = findRepeat (M.insert xdxs n m) (n+1) $ step xdxs
energy :: [Int] -> Int
energy xyz = sum $ abs <$> xyz
main = do
  input <- ((((read <$>) . words . (chFilter <$>)) <$>) . lines) <$> readFile "in/12.txt"
  let starts, finals :: [[(Int,Int)]]
      starts = ((,0) <$>) <$> transpose input
      finals = ((!! 1000) . iterate step) <$> starts
      repeats = findRepeat M.empty 0 <$> starts
      -- The input has the hidden property that the dimensional cycles all begin at their start
      partB = if any ((/=0) . fst) repeats then error "oops" else foldr1 lcm $ snd <$> repeats
  print $ sum [ energy xyz * energy dxyz | (xyz,dxyz) <- unzip <$> transpose finals ] -- part A
  print partB
