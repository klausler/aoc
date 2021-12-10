import Data.Array
import Data.Char(chr, ord)
import Data.List(sort)
import qualified Data.Set as S
neighbors arr r c = concat [[ (r-1,c) | r > 1], [ (r+1,c) | r < rows ], [ (r,c-1) | c > 1 ], [ (r,c+1) | c < cols ]]
  where ((1,1),(rows,cols)) = bounds arr
isLow arr ((r,c),e) = and [ e < arr ! n | n <- neighbors arr r c ]
reaches arr soFar rest
  | S.null rest = soFar
  | otherwise = reaches arr (pt `S.insert` soFar) (pt `S.delete` (rest `S.union` (S.fromList ups)))
  where
    pt@(r,c) = head $ S.toList rest
    ht = arr ! pt
    ups = [ n | n <- neighbors arr r c, arr ! n > ht, n `S.notMember` soFar ]
main = do
  lns <- lines <$> readFile "in/09.txt"
  let rows = length lns
      cols = length $ head lns
      arr = listArray ((1,1),(rows,cols)) $ concatMap ((subtract (ord '0') . ord) <$>) lns
      lows = filter (isLow arr) $ assocs arr
      reachables = [ reaches arr S.empty (S.fromList [low]) | (low, _) <- lows ]
      basins = [[ r | r <- S.toList reach, arr ! r < 9, null $ tail $ filter (r `S.member`) reachables ] | (low, reach) <- zip lows reachables ]
  print $ sum [ e + 1 | (_,e) <- lows ] -- part 1
  print $ product $ take 3 $ reverse $ sort $ length <$> basins -- part 2
