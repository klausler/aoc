import Data.Char(isLower,isUpper,toLower)
import qualified Data.Map as M
import Data.Maybe(maybeToList)
import qualified Data.Set as S
neighbors m (r,c) = filter (`M.member` m) [ (r-1,c), (r,c+1), (r+1,c), (r,c-1) ]
collectKeys m path = S.fromList $ filter isLower $ (m M.!) <$> path
explore m seen result [] [] = result
explore m seen result [] pool = explore m seen result pool []
explore m seen result (p:ps) pool
  | isLower atCh, Nothing <- M.lookup need paths = explore m seen' result' ps pool'
  | otherwise = explore m seen' result ps pool'
  where at = head p
        atCh = m M.! at
        forwardP = reverse p
        need = keysNeeded m S.empty S.empty forwardP
        seen' = S.union seen $ S.fromList new
        new = [ n | n <- neighbors m at, n `S.notMember` seen ]
        paths = M.findWithDefault M.empty atCh result
        paths' = M.insert need (length p - 1, collectKeys m p) paths
        result' = M.insert atCh paths' result
        pool' = ((:p) <$> new) ++ pool
keysNeeded m have need [] = need
keysNeeded m have need (at:rest)
  | isLower atCh = keysNeeded m (S.insert atCh have) need rest
  | isUpper atCh, (key `S.notMember` have) = keysNeeded m have (S.insert key need) rest
  | otherwise = keysNeeded m have need rest
  where atCh = m M.! at
        key = toLower atCh
allTos m from = explore m S.empty M.empty [] [[from]]
allPaths m locs starts = foldl addFrom M.empty $ starts ++ M.elems locs
  where addFrom fromTab from = M.insert from (allTos m from) fromTab
data State = State { len :: Int, ats :: S.Set (Int,Int), holding :: S.Set Char } deriving(Eq,Ord)
search tables m locs keys states
  | null need = len s
  | otherwise = search tables m locs keys $ S.union states' $ S.fromList new
  where (s,states') = S.deleteFindMin states
        need = S.toList $ keys S.\\ holding s
        new = [ State (len s + lenP) (S.fromList $ to:rest) (S.union (holding s) newKeys)
              | (at,rest) <- select $ S.toList $ ats s, k <- need, let to = locs M.! k
              , t <- maybeToList $ M.lookup k $ tables M.! at
              , (ifKeys, (lenP, newKeys)) <- M.assocs t
              , ifKeys `S.isSubsetOf` holding s ]
select [x] = [(x,[])]
select (x:xs) = (x,xs) : [ (y,x:ys) | (y,ys) <- select xs ]
main = do
  input <- lines <$> readFile "in/18.txt"
  let m = M.fromList $ concat [ [ ((r,c),ch) | (c,ch) <- zip [0..] ln, ch /= '#' ] | (r,ln) <- zip [0..] input ]
      locs = M.fromList $ concat [ [ (ch,(r,c)) | (c,ch) <- zip [0..] ln, isLower ch ] | (r,ln) <- zip [0..] input ]
      keys = S.fromList $ filter isLower $ M.keys locs
      [(startA@(saR,saC))] = [ rc | (rc,ch) <- M.assocs m, ch == '@' ]
      tablesA = allPaths m locs [startA]
      startsB = [ (saR-1,saC-1), (saR-1,saC+1), (saR+1,saC+1), (saR+1,saC-1) ]
      mB = foldl (\m at -> M.delete at m) m $ startA : neighbors m startA
      tablesB = allPaths mB locs startsB
  print $ search tablesA m locs keys $ S.singleton $ State 0 (S.singleton startA) S.empty -- part A
  print $ search tablesB mB locs keys $ S.singleton $ State 0 (S.fromList startsB) S.empty -- part B
