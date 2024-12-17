import qualified Data.Map as M
import qualified Data.Set as S
turnRight (dr,dc) = (dc,-dr)
hasLoop board soFar (atRC@(r,c)) (dir@(dr,dc))
  | board M.! atRC' == '*' = False
  | board M.! atRC' == '#' = hasLoop board soFar atRC $ turnRight dir
  | (atRC',dir) `S.member` soFar = True
  | otherwise = hasLoop board (S.insert (atRC',dir) soFar) atRC' dir
  where atRC' = (r+dr,c+dc)
countLoops _ _ n [] = n
countLoops board startRC n (at:rest)
  | board M.! at == '.'
  , hasLoop (M.insert at '#' board) (S.singleton (startRC,(-1,0))) startRC (-1,0)
    = countLoops board startRC (n+1) rest
  | otherwise = countLoops board startRC n rest
main = do
  lns <- lines <$> readFile "in/06.txt"
  let maxLen = maximum $ length <$> lns
      horiz = take maxLen $ repeat '*'
      bordered = map (\x->('*':x)++"*") $ horiz : lns ++ [horiz]
      board = M.fromList [ ((r,c),ch) | (r,ln) <- zip [1..] bordered, (c,ch) <- zip [1..] ln ]
      (startRC:_) = [ (r,c) | (r,ln) <- zip [1..] bordered, (c,ch) <- zip [1..] ln, ch == '^' ]
  print $ countLoops board startRC 0 $ M.keys board -- part 2
