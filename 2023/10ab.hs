import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe(maybeToList)
neighbors (r,c) '|' = [(r-1,c), (r+1,c)]
neighbors (r,c) '-' = [(r,c-1), (r,c+1)]
neighbors (r,c) 'J' = [(r,c-1), (r-1,c)]
neighbors (r,c) '7' = [(r,c-1), (r+1,c)]
neighbors (r,c) 'F' = [(r,c+1), (r+1,c)]
neighbors (r,c) 'L' = [(r,c+1), (r-1,c)]
neighbors (r,c) 'S' = [(r-1,c), (r,c-1), (r+1,c), (r,c+1)]
neighbors _ _ = []
travel graph at last = at : travel graph (head $ filter (/=last) $ graph M.! at) at
-- A point is enclosed if any ray therefrom crosses the loop an odd number of times
lookEast m (r,c)
  | Just ch <- M.lookup (r,c) m = ch : lookEast m (r,c+1)
  | otherwise = ""
isEnclosed "" = False
isEnclosed ('|':rest) = not $ isEnclosed rest
isEnclosed ('L':rest) | ('7':rest') <- dropWhile (=='-') rest = not $ isEnclosed rest'
isEnclosed ('F':rest) | ('J':rest') <- dropWhile (=='-') rest = not $ isEnclosed rest'
isEnclosed (_:rest) = isEnclosed rest
replaceStart start [n1,n2] = head [ ch | ch <- "|-J7FL", let ns = neighbors start ch,
                                         ns == [n1,n2] || ns == [n2,n1] ]
main = do
  lns <- lines <$> readFile "in/10.txt"
  let raw = M.fromList $ concat [ [((r,c),ch) | (c,ch) <- zip [0..] ln] | (r,ln) <- zip [0..] lns ]
      graph = M.fromList [ (at,[to | to <- neighbors at atch,
                                     toch <- maybeToList $ M.lookup to raw,
                                     at `elem` neighbors to toch])
                         | (at,atch) <- M.assocs raw ]
      [start] = [ at | (at,'S') <- M.assocs raw ]
      trip = takeWhile (/=start) $ tail $ travel graph start start
      inPipe = S.fromList (start : trip)
      raw' = M.mapWithKey (\at ch -> if at `S.member` inPipe then ch else '.') raw
      replacedStart = M.insert start (replaceStart start $ graph M.! start) raw'
  print $ S.size inPipe `div` 2 -- part A
  print $ sum [ 1 | (at,'.') <- M.assocs raw', isEnclosed $ lookEast replacedStart at ] -- part B
