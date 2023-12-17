import Data.Array
import qualified Data.Set as S
travel arr set (r,c) (dr,dc)
  | ((r,c),(dr,dc)) `S.member` set = set
  | ch == '#' = set
  | ch == '.' = proceed dr dc
  | ch == '/' = proceed (-dc) (-dr)
  | ch == '\\' = proceed dc dr
  | ch == '-', dr == 0 = proceed 0 dc
  | ch == '|', dc == 0 = proceed dr 0
  | ch == '-' = travel arr (proceed 0 (-1)) (r,c+1) (0,1)
  | ch == '|' = travel arr (proceed (-1) 0) (r+1,c) (1,0)
  where ch = arr ! (r,c)
        proceed dr' dc' = travel arr (S.insert ((r,c),(dr,dc)) set) (r+dr',c+dc') (dr',dc')
main = do
  lns <- lines <$> readFile "in/16.txt"
  let (rows,cols) = (length lns, maximum $ length <$> lns)
      hBorder = take (cols+2) $ repeat '#'
      wrapped = hBorder ++ concat [('#':(ln++"#")) | ln <-lns] ++ hBorder
      arr = listArray ((0,0),(rows+1,cols+1)) wrapped
      try r c dr dc = S.size $ S.map fst $ travel arr S.empty (r,c) (dr,dc)
  print $ try 1 1 0 1 -- part A
  print $ maximum $ [ try r 1 0 1 | r<-[1..rows] ] ++ [ try r cols 0 (-1) | r<-[1..rows] ] ++
                    [ try 1 c 1 0 | c<-[1..cols] ] ++ [ try rows c (-1) 0 | c<-[1..cols] ] -- part B
