pointsA (x,y) [] = [(x,y)]
pointsA (x,y) ((dirCh,n,_):rest) = (x,y) : pointsA (x+n*dx,y+n*dy) rest
  where
    (dx,dy) = dir dirCh
    dir 'U' = (-1,0); dir 'R' = (0,1); dir 'D' = (1,0); dir 'L' = (0,-1)
pointsB (x,y) [] = [(x,y)]
pointsB (x,y) ((_,_,hex):rest) = (x,y) : pointsB (x+n*dx,y+n*dy) rest
  where
    (dx,dy) = dir $ last hex
    n = read $ "0x" ++ init hex
    dir '3' = (-1,0); dir '0' = (0,1); dir '1' = (1,0); dir '2' = (0,-1)
trapezoidAreaX2 [_] = 0
trapezoidAreaX2 ((x,y):(rest@((x',y'):_))) = (y+y')*(x-x') + trapezoidAreaX2 rest
boundaryPts [_] = 0
boundaryPts ((x,y):(rest@((x',y'):_)))
  | x == x' = abs (y'-y) + boundaryPts rest
  | otherwise = abs (x'-x) + boundaryPts rest
picksTheorem points = boundary + interior
  where
    area = (abs $ trapezoidAreaX2 points) `div` 2
    boundary = boundaryPts points
    interior = area + 1 - boundary `div` 2
main = do
  lns <- lines <$> readFile "in/18.txt" -- "t.in"
  let input = [ (dir, read n, init rest)
              | ln <- lns, let [[dir], n, ('(':'#':rest)] = words ln ]
  print $ picksTheorem $ pointsA (0,0) input
  print $ picksTheorem $ pointsB (0,0) input
