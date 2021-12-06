import qualified Data.Map as M
decode [opc,'-':x] = (head opc, negate $ read x)
decode [opc,'+':x] = (head opc, read x)
run prog pc acc
  | Just (o,n) <- M.lookup pc prog = next o n
  | otherwise = acc
  where
    run' = run $ M.delete pc prog
    next 'n' _ = run' (pc+1) acc
    next 'a' n = run' (pc+1) (acc+n)
    next 'j' n = run' (pc+n) acc
main = do
  txt <- readFile "in/08.txt"
  let
    prog = M.fromList $ zip [0..] (map (decode.words) $ lines txt)
  print $ run prog 0 0
