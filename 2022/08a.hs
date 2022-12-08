import Data.Char(ord)
import Data.List(transpose)
import qualified Data.Set as S
toInt ch = ord ch - ord '0'
scan x y dx dy pts = S.fromList $ scan' x y (-1) pts
  where scan' _ _ _ [] = []
        scan' x y m (t:ts)
          | t > m = (x,y) : scan' (x+dx) (y+dy) t ts
          | otherwise = scan' (x+dx) (y+dy) m ts
main = do
  lns <- lines <$> readFile "in/08.txt"
  let input = (toInt <$>) <$> lns
      h = length input
      w = length $ head input
      points = S.unions [ S.unions [ scan 1 y 1 0 row | (y,row) <- zip [1..] input ],
                          S.unions [ scan w y (-1) 0 $ reverse row | (y,row) <- zip [1..] input ],
                          S.unions [ scan x 1 0 1 col | (x,col) <- zip [1..] $ transpose input ],
                          S.unions [ scan x h 0 (-1) $ reverse col | (x,col) <- zip [1..] $ transpose input ]]
  print $ S.size points
