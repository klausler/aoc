import qualified Data.Set as S
puzzlePart = 1 -- set to 2 for part two
getX (x,_,_,_) = x
getY (_,y,_,_) = y
getZ (_,_,z,_) = z
getW (_,_,_,w) = w
neighbors (x,y,z,w) = [ (x',y',z',w') | x' <- [x-1 .. x+1], y' <- [y-1 .. y+1], z' <- [z-1 .. z+1], w' <- [w-1 .. w+1], (x',y',z',w') /= (x,y,z,w) ]
halo pts = [ (x,y,z,w) | x <- expandedX, y <- expandedY, z <- expandedZ, w <- expandedW ]
  where
    expand f = let ns = map f pts in [minimum ns - 1 .. maximum ns + 1]
    (expandedX, expandedY, expandedZ, expandedW) = (expand getX, expand getY, expand getZ, if puzzlePart == 2 then expand getW else getW <$> pts)
step pts = filter isActive $ halo pts
  where
    set = S.fromList pts
    isActive p = ct == 3 || (ct == 2 && p `S.member` set)
      where ct = length $ filter (`S.member` set) $ neighbors p
main = do
  txt <- readFile "in/17.txt"
  let
    start = [ (r,c,0,0) | (r,ln) <- zip [0..] $ lines txt,
              (c,ch) <- zip [0..] ln, ch == '#' ]
  print $ length $ iterate step start !! 6
