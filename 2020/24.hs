import qualified Data.Set as S
toEast = (`mod` 2)
mv (r,c) "" = (r,c)
mv (r,c) ('e':rest) = mv (r,c+1) rest
mv (r,c) ('w':rest) = mv (r,c-1) rest
mv (r,c) (ns:ew:rest) = mv (r + r', c + c') rest
  where r' = if ns == 'n' then -1 else 1
        c' = toEast r + (if ew == 'e' then 0 else -1)
doTile dirs set = if fP `S.member` set then S.delete fP set else S.insert fP set
  where fP = mv (0,0) dirs
halo set = [ (r,c) | r <- [minimum rs - 1 .. maximum rs + 1 ], c <- [ cMin - 1 .. cMax + 1 ] ]
  where (rs,cs) = unzip $ S.toList set
        (cMin,cMax) = (minimum cs, maximum cs)
neighbors (r,c) =  [ (r-1, east-1), (r-1, east), (r,c-1), (r,c+1), (r+1,east-1), (r+1,east) ]
  where east = c + toEast r
step set = S.fromList $ filter toBlack $ halo set
  where toBlack pt = nB == 2 || (nB == 1 && pt `S.member` set)
          where nB = length $ filter (`S.member` set) $ neighbors pt
steps = 100 -- use 0 for part one, 100 for part two
main = do
  txt <- readFile "in/24.txt"
  print $ S.size $ (iterate step $ foldr doTile S.empty $ lines txt) !! steps
