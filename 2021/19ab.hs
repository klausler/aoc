import Data.List(groupBy)
import qualified Data.Set as S
grouper l1 l2 = null l1 == null l2
rdLine :: String -> (Int,Int,Int)
rdLine ln = read $ "((" ++ (concatMap (\c->if c == ',' then "),(" else [c]) ln) ++ "))"
orientations = [ first . up
               | first <- [noop, \(x,y,z)->(y,-x,z), \(x,y,z)->(-x,-y,z), \(x,y,z)->(-y,x,z)
                          , \(x,y,z)->(z,y,-x), \(x,y,z)->(-z,y,x) ],
                 up <- [noop, \(x,y,z)->(x,z,-y), \(x,y,z)->(x,-y,-z), \(x,y,z)->(x,-z,y) ] ]
  where noop x = x
offset (dx,dy,dz) (x,y,z) = (x+dx,y+dy,z+dz)
pairsWith set beacons
  | (xf:_) <- hits = Just xf
  | otherwise = Nothing
  where xforms = [ offset coord . o | (x,y,z) <- S.toList set, b <- beacons, o <- orientations
                                    , let (x',y',z') = o b, let coord = (x-x',y-y',z-z') ]
        hits = [ xf | xf <- xforms, length (filter (`S.member` set) (xf <$> beacons)) >= 12 ]
extract :: (a -> Maybe b) -> [a] -> Maybe (a, b, [a])
extract _ [] = Nothing
extract f (x:xs)
  | Just y <- f x = Just (x, y, xs)
  | Just (z, y, rest) <- extract f xs = Just (z, y, x:rest)
  | otherwise = Nothing
solve set coords [] = (set, coords)
solve set coords rest = solve set' (xf (0,0,0):coords) rest'
  where
    Just (beacons, xf, rest') = extract (pairsWith set) rest
    set' = set `S.union` (S.fromList $ xf <$> beacons)
allPairs [] = []
allPairs (x:xs) = [ (x,y) | y <- xs ] ++ allPairs xs
manhattan (x,y,z) (x',y',z') = abs (x-x') + abs (y-y') + abs (z-z')
main = do
  lns <- lines <$> readFile "in/19.txt"
  let grps = filter (not.null.head) $ groupBy grouper lns
      (scan0:scanners) = ((rdLine <$>) . tail) <$> grps
      (solved,coords) = solve (S.fromList scan0) [(0,0,0)] scanners
  print $ S.size solved -- part 1
  print $ maximum $ uncurry manhattan <$> allPairs coords -- part 2
