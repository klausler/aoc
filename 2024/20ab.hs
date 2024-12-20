import qualified Data.Map as M
import qualified Data.Set as S
survey f lns = [ (r,c) | (r,ln) <- zip [1..] lns, (c,ch) <- zip [1..] ln, f ch ]
halo d (r,c) = [ (r+dr,c+dc) | dr <- [-d..d]
               , let adr = abs dr, dc <- [(adr-d)..(d-adr)] ]
flood _ dists _ [] = dists
flood space dists d work = flood space dists' (d+1) work'
  where
    dists' = foldr (\rc ds->M.insert rc d ds) dists work
    next = [ rc' | rc <- work , rc' <- halo 1 rc, rc' /= rc
           , rc' `S.member` space
           , rc' `M.notMember` dists' ]
    work' = S.toList $ S.fromList next
manhattan (r,c) (r',c') = abs (r-r') + abs (c-c')
cheats maxd mind dists = sum
  [ 1 | (rc,d) <- M.assocs dists, rc' <- halo maxd rc
  , let d' = manhattan rc rc'
           + M.findWithDefault 999999 rc' dists
  , d >= d' + mind ]
main = do
  lns <- lines <$> readFile "in/20.txt"
  let space = S.fromList $ survey (/= '#') lns
      ([start],[end]) = (survey (=='S') lns, survey (=='E') lns)
      dists = flood space M.empty 0 [end]
  print $ cheats 2 100 dists -- part 1
  print $ cheats 20 100 dists -- part 2
