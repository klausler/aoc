module IntCode(load, run) where
import qualified Data.Map as M
load :: String -> M.Map Int Int
run :: M.Map Int Int -> Int -> [Int] -> (M.Map Int Int, [Int])
load str = M.fromList $ zip [0..] $ read $ '[' : str ++ "]"
run m at input
  | opc == 99 = (m, [])
  | opc == 1 = dyadic (+)
  | opc == 2 = dyadic (*)
  | opc == 3 = run (M.insert opd1 (head input) m) (at + 2) (tail input)
  | opc == 4 = let (m',o') = run m (at + 2) input in (m', left : o')
  | opc == 5 = branch (/= 0)
  | opc == 6 = branch (== 0)
  | opc == 7 = dyadic $ truth (<)
  | opc == 8 = dyadic $ truth (==)
  where op = m M.! at
        (modes,opc) = op `divMod` 100
        [opd1, opd2, opd3] = [ m M.! (at + j) | j <- [1..3] ]
        (modes', leftMode) = modes `divMod` 10
        (modes'', rightMode) = modes' `divMod` 10
        get 0 opd = m M.! opd
        get 1 opd = opd
        left = get leftMode opd1
        right = get rightMode opd2
        dyadic f = run (M.insert opd3 (f left right) m) (at + 4) input
        truth f x y = if f x y then 1 else 0
        branch f = run m (if f left then right else at + 3) input
