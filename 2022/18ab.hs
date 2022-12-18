import qualified Data.Set as S
dirs = [(-1,0,0), (1,0,0), (0,-1,0), (0,1,0), (0,0,-1), (0,0,1)]
ptAdd (x,y,z) (dx,dy,dz) = (x+dx,y+dy,z+dz)
area set pt = sum [ 1 | d <- dirs, S.notMember (ptAdd pt d) set ]
permeate m set gasSet queue
  | S.null queue = gasSet
  | S.member p set = skip
  | S.member p gasSet = skip
  | x < -1 || x > m || y < -1 || y > m || z < -1 || z > m = skip
  | otherwise = permeate m set (S.insert p gasSet) (S.delete p $ S.union queue (S.fromList (ptAdd p <$> dirs)))
  where (p@(x,y,z)) = head $ S.toList queue
        skip = permeate m set gasSet $ S.delete p queue
extArea set gasSet pt = sum [ 1 | d <- dirs, S.member (ptAdd pt d) gasSet ]
main = do
  pts <- (S.fromList . ((read . ('(':) . (++")")) <$>) . lines) <$> readFile "in/18.txt"
  print $ sum $ area pts <$> S.toList pts -- part A
  let m = maximum $ concat [ [x,y,z] | (x,y,z) <- S.toList pts ]
      gasSet = permeate (m+1) pts S.empty $ S.singleton (0,0,0)
  print $ sum $ extArea pts gasSet <$> S.toList pts -- part B
