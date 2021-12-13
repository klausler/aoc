import qualified Data.Set as S
max2 (x1,y1) (x2,y2) = (x1 `max` x2, y1 `max` y2)
limits = foldr1 max2 . S.toList
doCmd dots c = if c !! 11 == 'x' then foldX else foldY
  where
    at = read $ drop 13 c
    (maxX, maxY) = limits dots
    foldX = S.map (\(x,y) -> (adjust x maxX, y)) dots
    foldY = S.map (\(x,y) -> (x, adjust y maxY)) dots
    adjust xy lim = let offset = (lim - 2 * at) `max` 0
                    in offset + if xy > at then at - (xy - at) else xy
main = do
  lns <- lines <$> readFile "in/13.txt"
  let (xys,(_:cmds)) = break null lns
      dots = S.fromList $ (read . ('(':) . (++")")) <$> xys
      folded = foldl doCmd dots cmds
      (maxX, maxY) = limits folded
  print $ S.size $ foldl doCmd dots $ take 1 cmds -- part 1
  putStrLn  $ concat [ '\n' : [ if (x,y) `S.member` folded then '#' else '.'
                              | x <- [0 .. maxX] ]
                     | y <- [0 .. maxY]] -- part 2
