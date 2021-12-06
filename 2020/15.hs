import qualified Data.Map as M
myInput = [15,5,1,4,7,0]
n = 2020 -- for part one; use 30000000 for part two
f turn last mem = last : (f (turn+1) (maybe 0 (turn-) $ M.lookup last mem) $ M.insert last turn mem)
start = init myInput
rest = f (length start) (last myInput) $ M.fromList $ zip start [0..]
main = print $ (start ++ rest) !! (n-1)
