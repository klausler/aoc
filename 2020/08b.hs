import qualified Data.Map as M
import Data.Maybe(maybeToList)
decode [opc,'-':x] = (head opc, negate $ read x)
decode [opc,'+':x] = (head opc, read x)
run prog pc acc
  | Just (o,n) <- M.lookup pc prog = next o n
  | otherwise = Nothing
  where
    run' = run $ M.delete pc prog
    next 'n' _ = run' (pc+1) acc
    next 'a' n = run' (pc+1) (acc+n)
    next 'j' n = run' (pc+n) acc
    next 's' _ = Just acc
try prog pc
  | Just ('n',n) <- inst = run (M.insert pc ('j',n) prog) 0 0
  | Just ('j',n) <- inst = run (M.insert pc ('n',n) prog) 0 0
  | otherwise = Nothing
  where
    inst = M.lookup pc prog
main = do
  txt <- readFile "input08.txt"
  let
    parsed = map (decode.words) $ lines txt
    prog = M.fromList $ zip [0..] (parsed ++ [('s',0)])
  print [ (pc,a) | pc <- [0..M.size prog - 2], a <- maybeToList $ try prog pc ]
