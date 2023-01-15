import IntCode
import qualified Data.Map as M
track m [] = m
track m (c:r:t:rest) = track (M.insert (r,c) t m) rest
play _ _ score [] = ([], score)
play pc bc score (c:r:t:rest)
  | c == -1, r == 0 = play pc bc t rest
  | t == 3 = play c bc score rest
  | t == 4 = let (input, result) = play pc c score rest in (joy:input, result)
  | otherwise = play pc bc score rest
  where joy = signum $ c - pc -- keep paddle under ball
main = do
  m <- load <$> readFile "in/13.txt"
  print $ length $ filter (==2) $ M.elems $ track M.empty $ snd $ run m 0 0 [] -- part A
  let (input, partB) = play 0 0 0 $ snd $ run (M.insert 0 2 m) 0 0 input
  print partB
