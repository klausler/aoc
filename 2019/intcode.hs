module IntCode(load, run) where
import qualified Data.Map as M
load :: [Int] -> M.Map Int Int
run :: M.Map Int Int -> Int -> Int
load = M.fromList . zip [0..]
run m at
  | op == 99 = m M.! 0
  | op == 1 = run (M.insert to (left + right) m) $ at + 4
  | op == 2 = run (M.insert to (left * right) m) $ at + 4
  where op = m M.! at
        left = m M.! (m M.! (at + 1))
        right = m M.! (m M.! (at + 2))
        to = m M.! (at + 3)
