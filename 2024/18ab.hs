import qualified Data.Map as M
import qualified Data.Set as S
astar isOk front done
  | S.null front = []
  | at==(70,70) = [dist]
  | otherwise = astar isOk (S.union front' $ S.fromList nexts) done'
  where
    ((dist,(at@(r,c))),front') = S.deleteFindMin front
    done' = at `S.insert` done
    nexts = [ (dist+1, at')
            | (dr,dc) <- [(-1,0),(0,1),(1,0),(0,-1)]
            , let at' = (r+dr,c+dc)
            , at' `S.notMember` done, isOk at' ]
lowest f lo hi
  | lo > hi = error "oops"
  | lo == hi = lo
  | f mid = lowest f lo mid
  | otherwise = lowest f (mid+1) hi
  where mid = lo + (hi - lo) `div` 2
main = do
  lns <- lines <$> readFile "in/18.txt"
  let bytes = [ read $ '(' : ln ++ ")" | ln <- lns ] :: [(Int, Int)]
      board = M.fromListWith const $ zip bytes [0..]
      within (r,c) = r >= 0 && r <= 70 && c >= 0 && c <= 70
      isOk lim at = within at && M.findWithDefault 9999 at board > lim
      solve lim = astar (isOk lim) (S.singleton (0,(0,0))) S.empty
  print $ head $ solve 1025 -- part 1
  print $ bytes !! (lowest (\n -> null $ solve n) 0 9999) -- part 2
