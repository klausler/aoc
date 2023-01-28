import qualified Data.Map as M
import qualified Data.Set as S
keep ch
  | ch == '\n' || ch >= '0' && ch <= '9' = ch
  | otherwise = ' '
overlap (x0a,x0b,y0a,y0b) (x1a,x1b,y1a,y1b) = not (indep x0a x0b x1a x1b || indep y0a y0b y1a y1b)
  where indep a b c d = b < c || d < a
main = do
  input <- (lines . (keep <$>)) <$> readFile "in/03.txt"
  let areas = [ (which,(x,x+xs-1,y,y+ys-1)) | [which,x,y,xs,ys] <- ((read <$>) . words) <$> input ]
      m = M.fromListWith S.union [ ((x,y), S.singleton which)
                                 | (which,(xa,xb,ya,yb)) <- areas, x <- [xa..xb], y <- [ya..yb] ]
  print $ length $ filter ((>1).S.size) $ M.elems m -- part A
  print [ which | (which,a) <- areas, all ((which ==) . fst) $ filter (overlap a . snd) areas ] -- part B
