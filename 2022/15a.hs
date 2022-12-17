import qualified Data.Set as S
rip = map (\ch -> if ch >= '0' && ch <= '9' || ch == '-' then ch else ' ')
parse ln = let [a,b,c,d] = read <$> (words $ rip ln) in ((a,b),(c,d))
manhattan (x,y) (x',y') = abs (x-x') + abs (y-y')
magicY = 2000000
overlap (x,y) d
  | w < 0 = S.empty
  | otherwise = S.fromList [(x-w)..(x+w)]
  where w = d - abs (y - magicY)
main = do
  input <- ((parse <$>) . lines) <$> readFile "in/15.txt"
  let sensors = [ (sxy, manhattan sxy bxy) | (sxy,bxy) <- input ]
      badBeacons = S.fromList [ x | (_,(x,y)) <- input, y == magicY ]
  print $ S.size $ (S.unions $ (uncurry overlap) <$> sensors) S.\\ badBeacons
