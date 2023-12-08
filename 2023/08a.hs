import qualified Data.Map as M
distance graph "ZZZ" _ = 0
distance graph at (turn:turns) = 1 + distance graph (which $ graph M.! at) turns
  where which = if turn == 'L' then fst else snd
main = do
  (turns:_:nodeLines) <- lines <$> readFile "in/08.txt"
  let graph = M.fromList [ (f,(init $ tail t1, init t2)) | [f,_,t1,t2] <- words <$> nodeLines ]
  print $ distance graph "AAA" $ cycle turns -- part A
