import Data.Array
import Data.Char(ord)
import Data.List(minimumBy)
import Data.Maybe(fromJust, isJust)
import qualified Data.Map as M
import qualified Data.Set as S
-- 0   1   2   3   4   5  6
--       7  11  15  19
--       8  12  16  20
--       9  13  17  21
--      10  14  18  22
hall = [0..6]
inRoom = (>=7)
whichRoom n = (n-7) `div` 4
slot n = (n-7) `mod` 4
leftExit n = 1 + whichRoom n
rightExit n = 1 + leftExit n
roomTop n = 7 + 4 * ((n-7) `div` 4)
distance :: Array Int Char -> Int -> Int -> Maybe Int
distance state to from
  | to == from = Just 0
  | state!to /= '.' = Nothing
  | otherwise = dist from
  where
    dist at
      | at == to = Just 0
      | at /= from, state!at /= '.' = Nothing
      | inRoom at, inRoom to, whichRoom at == whichRoom to = (+1) <$> dist (if to > at then at+1 else at-1)
      | inRoom at, slot at > 0 = (+1) <$> dist (at-1)
      | inRoom at, to > at = (+2) <$> dist (rightExit at)
      | inRoom at, not (inRoom to), to > leftExit at = (+2) <$> dist (rightExit at)
      | inRoom at = (+2) <$> dist (leftExit at)
      | at == 0 = (+1) <$> dist 1
      | at == 1, to == 0 = (+1) <$> dist 0
      | at == 6 = (+1) <$> dist 5
      | at == 5, to == 6 = (+1) <$> dist 6
      | inRoom to, at < leftExit to = (+2) <$> dist (at+1)
      | inRoom to, at > rightExit to = (+2) <$> dist (at-1)
      | inRoom to = (+2) <$> dist (roomTop to)
      | to > at = (+2) <$> dist (at+1)
      | otherwise = (+2) <$> dist (at-1)
dests :: Array Int Char -> Int -> [Int]
dests state from
  | inRoom from, whichRoom from == whichRoom toRoom = if toRoom == toRoomTop then [] else hall
  | from < 7 = [toRoom]
  | otherwise = toRoom : hall
  where
    which = state!from
    toRoomTop = 7 + 4 * (ord which - ord 'A')
    toRoom = if state!(toRoomTop+3) /= which then toRoomTop+3
             else if state!(toRoomTop+2) /= which then toRoomTop+2
             else if state!(toRoomTop+1) /= which then toRoomTop+1
             else toRoomTop
tenPows = iterate (10*) 1
moves :: Int -> Array Int Char -> [(Array Int Char, Int)]
moves cost state = [ (state // [(from,'.'),(to,which)],
                      cost + fromJust d * energy which)
                   | (from,which) <- assocs state, which /= '.'
                   , to <- dests state from, let d = distance state to from, isJust d ]
  where energy which = tenPows !! (ord which - ord 'A')
dijkstra :: M.Map (Array Int Char) Int -> S.Set (Array Int Char) -> Int
dijkstra frontier visited
  | (drop 7 $ elems state) == "AAAABBBBCCCCDDDD" = cost
  | otherwise = dijkstra frontier' visited'
  where
    cmp (_,d1) (_,d2) = compare d1 d2
    (state,cost) = minimumBy cmp $ M.assocs frontier
    visited' = S.insert state visited
    frontier' = M.unionWith min (M.delete state frontier) $ M.fromList $
       filter ((`S.notMember` visited') . fst) $ moves cost state
main = do
  [_,_,('#':'#':'#':a1:'#':b1:'#':c1:'#':d1:_),(' ':' ':'#':a2:'#':b2:'#':c2:'#':d2:_),_] <- lines <$> readFile "in/23.txt"
  let part1state0 = listArray (0,22) $ "......." ++ [a1,a2,'A','A',b1,b2,'B','B',c1,c2,'C','C',d1,d2,'D','D']
      part2state0 = listArray (0,22) $ "......." ++ [a1,'D','D',a2,b1,'C','B',b2,c1,'B','A',c2,d1,'A','C',d2]
  print $ dijkstra (M.fromList $ moves 0 part1state0) S.empty
  print $ dijkstra (M.fromList $ moves 0 part2state0) S.empty
