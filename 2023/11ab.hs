import qualified Data.Set as S
pairs [] = []
pairs (x:xs) = [(x,x') | x' <- xs] ++ pairs xs
main = do
  lns <- lines <$> readFile "in/11.txt"
  let emptyCols = S.toList $ foldr1 S.intersection
        [ S.fromList [ c | (c,ch) <- zip [1..] ln, ch /= '#' ] | ln <- lns ]
      emptyRows = [ r | (r,ln) <- zip [1..] lns, all (/='#') ln ]
      expand factor list coord = coord + (factor-1) * length (takeWhile (<coord) list)
      galaxies factor = concat [ [(expand factor emptyRows r, expand factor emptyCols c)
                                 | (c,ch) <- zip [1..] ln, ch == '#']
                               | (r,ln) <- zip [1..] lns ]
  print $ sum [ abs (r-r') + abs (c-c') | ((r,c),(r',c')) <- pairs $ galaxies 2 ] -- part A
  print $ sum [ abs (r-r') + abs (c-c') | ((r,c),(r',c')) <- pairs $ galaxies 1000000 ] -- part B
