import Data.Char(isDigit)
import Data.List(elemIndex)
import qualified Data.Map as M
pad ln cols = take cols $ ln ++ repeat ' '
next m rows cols dr dc r c
  | r < 1 = next m rows cols dr dc rows c
  | r > rows = next m rows cols dr dc 1 c
  | c < 1 = next m rows cols dr dc r cols
  | c > cols = next m rows cols dr dc r 1
  | m M.! (r,c) /= ' ' = (r,c)
  | otherwise = next m rows cols dr dc (r+dr) (c+dc)
travel m rows cols start = travel' start (0,1)
  where travel' (r,c) (dr,dc) "" = ((r,c),(dr,dc))
        travel' (r,c) (0,1) ('L':rest) = travel' (r,c) (-1,0) rest
        travel' (r,c) (1,0) ('L':rest) = travel' (r,c) (0,1) rest
        travel' (r,c) (0,-1) ('L':rest) = travel' (r,c) (1,0) rest
        travel' (r,c) (-1,0) ('L':rest) = travel' (r,c) (0,-1) rest
        travel' (r,c) (0,1) ('R':rest) = travel' (r,c) (1,0) rest
        travel' (r,c) (1,0) ('R':rest) = travel' (r,c) (0,-1) rest
        travel' (r,c) (0,-1) ('R':rest) = travel' (r,c) (-1,0) rest
        travel' (r,c) (-1,0) ('R':rest) = travel' (r,c) (0,1) rest
        travel' (r,c) (dr,dc) dist = travel' (step (read nStr) r c) (dr,dc) rest
          where (nStr,rest) = span isDigit dist
                step 0 r c = (r,c)
                step n r c
                  | m M.! (r',c') == '#' = (r,c)
                  | otherwise = step (n-1) r' c'
                  where (r',c') = next m rows cols dr dc (r+dr) (c+dc)
main = do
  input <- lines <$> readFile "in/22.txt"
  let rows = length input - 2
      cols = maximum $ length <$> take rows input
      m = M.fromList [ ((r,c),ch) | (r,ln) <- zip [1..] $ take rows input, (c,ch) <- zip [1..] $ pad ln cols ]
      start = next m rows cols 0 1 1 1
      ((r',c'),dir') = travel m rows cols start $ last input
      Just dirScore = elemIndex dir' [(0,1),(1,0),(0,-1),(-1,0)]
  print $ 1000 * r' + 4 * c' + dirScore
