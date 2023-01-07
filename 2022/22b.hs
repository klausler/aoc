import Data.Char(isDigit)
import Data.List(elemIndex)
import qualified Data.Map as M
pad ln cols = take cols $ ln ++ repeat ' '
next m rows cols (dr,dc) (r,c) = next' (dr,dc) (r,c)
  where (unit,0) = rows `divMod` 4
        (3,0) = cols `divMod` unit
        next' (dr,dc) (r,c)
--  .BR  Right edge of U joins bottom edge of R
--  .U.  Left edge of U joins top edge of L
--  LF.  Right edge of F joins right edge of R reversed
--  D..  Right edge of D joins bottom edge of F
--       Left edge of D joins top edge of B
--       Bottom edge of D joins top edge of R
--       Left edge of L joins left edge of B reversed
          | r == 0, c > unit, c <= 2*unit, dr == -1 = next' (0,1) (3*unit+c-unit,1) -- B top -> D left
          | r == 0, c > 2*unit, dr == -1 = next' (-1,0) (4*unit,c-2*unit) -- R top -> D bottom
          | r >= 1, r <= unit, c == unit, dc == -1 = next' (0,1) (3*unit+1-r,1) -- B left -> L left
          | r >= 1, r <= unit, c > 3*unit, dc == 1 = next' (0,-1) (3*unit+1-r, 2*unit) -- R right -> F right
          | r == unit+1, c > 2*unit, dr == 1 = next' (0,-1) (c - unit, 2*unit) -- R bottom -> U right
          | r > unit, r <= 2*unit, c == unit, dc == -1 = next' (1,0) (2*unit+1,r-unit) -- U left -> L top
          | r > unit, r <= 2*unit, c == 2*unit+1, dc == 1 = next' (-1,0) (unit,unit+r) -- U right -> R bottom
          | r == 2*unit, c >= 1, c <= unit, dr == -1 = next' (0,1) (unit+c,unit+1) -- L top -> U left
          | r > 2*unit, r <= 3*unit, c == 0, dc == -1 = next' (0,1) (3*unit+1-r,unit+1) -- L left -> B left
          | r > 2*unit, r <= 3*unit, c == 2*unit+1, dc == 1 = next' (0,-1) (3*unit+1-r, 3*unit) -- F right -> R right
          | r == 3*unit+1, c > unit, c <= 2*unit, dr == 1 = next' (0,-1) (2*unit+c,unit) -- F bottom -> D right
          | r > 3*unit, r <= 4*unit, c == unit+1, dc == 1 = next' (-1,0) (3*unit,r-2*unit) -- D right -> F bottom
          | r > 3*unit, r <= 4*unit, c == 0, dc == -1 = next' (1,0) (1,r-2*unit) -- D left -> B top
          | r == 4*unit+1, c <= unit, dr == 1 = next' (1,0) (1,2*unit+c) -- D bottom -> R top
          | otherwise = ((r,c),(dr,dc))
travel m rows cols start dir path = travel' start dir path
  where travel' (r,c) (dr,dc) "" = [((r,c),(dr,dc))]
        travel' (r,c) (0,1) ('L':rest) = ((r,c),(0,1)) : travel' (r,c) (-1,0) rest
        travel' (r,c) (1,0) ('L':rest) = ((r,c),(1,0)) : travel' (r,c) (0,1) rest
        travel' (r,c) (0,-1) ('L':rest) = ((r,c),(0,-1)) : travel' (r,c) (1,0) rest
        travel' (r,c) (-1,0) ('L':rest) = ((r,c),(-1,0)) : travel' (r,c) (0,-1) rest
        travel' (r,c) (0,1) ('R':rest) = ((r,c),(0,1)) : travel' (r,c) (1,0) rest
        travel' (r,c) (1,0) ('R':rest) = ((r,c),(1,0)) : travel' (r,c) (0,-1) rest
        travel' (r,c) (0,-1) ('R':rest) = ((r,c),(0,-1)) : travel' (r,c) (-1,0) rest
        travel' (r,c) (-1,0) ('R':rest) = ((r,c),(-1,0)) : travel' (r,c) (0,1) rest
        travel' (r,c) (dr,dc) dist = ((r,c),(dr,dc)) : travel' rc' drdc' rest
          where (nStr,rest) = span isDigit dist
                (rc',drdc') = step (read nStr) r c dr dc
                step 0 r c dr dc = ((r,c),(dr,dc))
                step n r c dr dc
                  | m M.! (r',c') == '#' = ((r,c),(dr,dc))
                  | otherwise = step (n-1) r' c' dr' dc'
                  where ((r',c'),(dr',dc')) = next m rows cols (dr,dc) (r+dr,c+dc)
main = do
  input <- lines <$> readFile "in/22.txt"
  let rows = length input - 2
      cols = maximum $ length <$> take rows input
      m = M.fromList [ ((r,c),ch) | (r,ln) <- zip [1..] $ take rows input, (c,ch) <- zip [1..] $ pad ln cols ]
      start = (1, (cols `div` 3) + 1)
      ((r',c'),dir') = last $ travel m rows cols start (0,1) $ last input
      Just dirScore = elemIndex dir' [(0,1),(1,0),(0,-1),(-1,0)]
  print $ 1000 * r' + 4 * c' + dirScore
