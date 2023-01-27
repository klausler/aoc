module IntCode(ExecutionStatus(..), executeSingle, load, run) where
import qualified Data.Map as M

data ExecutionStatus = Normal | Done | NeedInput deriving(Eq)

load :: String -> M.Map Int Int
load str = M.fromList $ zip [0..] $ read $ '[' : str ++ "]"

run :: M.Map Int Int -> Int -> Int -> [Int] -> (M.Map Int Int, [Int])
run m at base input
  | status == Normal = let (m'', output') = run m' at' base' input' in (m'', output ++ output')
  | otherwise = (m', output)
  where (m',at',base',input',output,status) = executeSingle m at base input

executeSingle :: M.Map Int Int -> Int -> Int -> [Int]
              -> (M.Map Int Int, Int, Int, [Int], [Int], ExecutionStatus)
executeSingle m at base input
  | opc == 99 = (m,at,base,input,[],Done)
  | opc == 1 = dyadic (+)
  | opc == 2 = dyadic (*)
  | opc == 3, null input = (m,at,base,[],[],NeedInput)
  | opc == 3 = (put leftMode opd1 $ head input, at + 2, base, tail input, [], Normal)
  | opc == 4 = (m, at + 2, base, input, [left], Normal)
  | opc == 5 = branch (/= 0)
  | opc == 6 = branch (== 0)
  | opc == 7 = dyadic $ truth (<)
  | opc == 8 = dyadic $ truth (==)
  | opc == 9 = (m, at + 2, base + left, input, [], Normal)
  | otherwise = error $ "execute failure: " ++ show (at,base,opc)
  where op = m M.! at
        (modes,opc) = op `divMod` 100
        load addr = M.findWithDefault 0 addr m
        [opd1, opd2, opd3] = [ load $ at + j | j <- [1..3] ]
        (modes', leftMode) = modes `divMod` 10
        (toMode, rightMode) = modes' `divMod` 10
        get 0 opd = load opd
        get 1 opd = opd
        get 2 opd = load $ base + opd
        left = get leftMode opd1
        right = get rightMode opd2
        put 0 addr val = M.insert addr val m
        put 2 addr val = M.insert (base+addr) val m
        dyadic f = (put toMode opd3 $ f left right, at + 4, base, input, [], Normal)
        truth f x y = if f x y then 1 else 0
        branch f = (m, if f left then right else at + 3, base, input, [], Normal)
