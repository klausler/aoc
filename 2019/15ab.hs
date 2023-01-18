import IntCode
import Data.List(elemIndex)
import qualified Data.Map as M
import qualified Data.Set as S
neighbors (r,c) = [ (r-1,c), (r+1,c), (r,c-1), (r,c+1) ] -- NSWE
step at to = let Just dir = elemIndex to $ neighbors at in dir + 1
steps at [] = []
steps at (to:rest) = step at to : steps to rest
explore set [] nextPoolMap output
  | null new = ([], [], set)
  | otherwise = explore set new M.empty output
  where new = [ p | p <- M.elems nextPoolMap, last p `S.notMember` set ]
explore set (path:paths) nPM output = (path1 ++ path2 ++ input', result, set'')
  where path1 = steps (0,0) path
        (ones,(found:output')) = splitAt (length path1 - 1) output
        pathBack = tail $ reverse $ (0,0) : path
        to = last path
        path2 | any (/=1) ones = error "unexpectedly drove into wall!"
              | found == 0 = steps (head pathBack) $ tail pathBack
              | otherwise = steps to pathBack
        (ones',output'') = splitAt (length path2) output'
        set' = if found == 0 then set else S.insert to set
        (input', result', set'') = explore set' paths nPM' output''
        result = if found == 2 then path else result'
        nPM' = M.unionWith const nPM $ M.fromList [ (n, path ++ [n]) | found == 1, n <- neighbors $ last path ]
flood n set filled frontier
  | S.null next = n
  | otherwise = flood (n+1) set (S.union filled frontier) next
  where filled' = S.union filled frontier
        next = (S.intersection set $ S.fromList $ S.toList frontier >>= neighbors) S.\\ filled'
main = do
  m <- load <$> readFile "in/15.txt"
  let (input, path, set) = explore (S.singleton (0,0)) [] (M.fromList [ (n,[n]) | n <- neighbors (0,0)]) output
      (_, output) = run m 0 0 input
  print $ length path -- part A
  print $ flood 0 set S.empty $ S.singleton $ last path -- part B
