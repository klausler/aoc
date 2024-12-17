import qualified Data.Map as M
import qualified Data.Set as S
turnRight (dr,dc) = (dc,-dr)
walk board soFar (atRC@(r,c)) (dir@(dr,dc))
  | board M.! atRC' == '*' = soFar
  | board M.! atRC' == '#' = walk board soFar atRC $ turnRight dir
  | otherwise = walk board (S.insert atRC' soFar) atRC' dir
  where atRC' = (r+dr,c+dc)
main = do
  lns <- lines <$> readFile "in/06.txt"
  let maxLen = maximum $ length <$> lns
      horiz = take maxLen $ repeat '*'
      bordered = map (\x->('*':x)++"*") $ horiz : lns ++ [horiz]
      board = M.fromList [ ((r,c),ch) | (r,ln) <- zip [1..] bordered, (c,ch) <- zip [1..] ln ]
      (startRC:_) = [ (r,c) | (r,ln) <- zip [1..] bordered, (c,ch) <- zip [1..] ln, ch == '^' ]
  print $ S.size $ walk board (S.singleton startRC) startRC (-1,0) -- part 1
