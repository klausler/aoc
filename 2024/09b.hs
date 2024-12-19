import Data.Char(ord)
import qualified Data.Map as M
import qualified Data.Set as S
type BSMap = M.Map Int [Int]
getFill :: BSMap -> S.Set Int -> Int -> (Maybe (Int,Int), BSMap)
getFill sized _ 0 = (Nothing, sized)
getFill sized set n
  | null nexts = nb
  | j `S.member` set = getFill sized' set n
  | Just (nj,_) <- smaller, nj > j = nb
  | otherwise = (Just (j,n), sized')
  where
     nexts = M.findWithDefault [] n sized
     (nb@(smaller, _)) = getFill sized set (n-1)
     (j:rest) = nexts
     sized' = M.insert n rest sized
process _ _ [] = []
process sized set ((block@(j,ct)):rest)
  | j >= 0, j `S.notMember` set = block : process sized (S.insert j set) rest
  | j >= 0 = (-1,ct) : process sized set rest
  | (Just (fill@(f,fct)), sized') <- getFill sized set ct =
    let set' = S.insert f set
    in fill : (process sized' set' $ [ (-1,ct-fct) | ct > fct ] ++ rest)
  | otherwise = block : process sized set rest
checksum n _ [] = n
checksum n at ((j,ct):rest) = checksum (n + (j `max` 0) * (sum [at..at+ct-1])) (at+ct) rest
main = do
  input <- init <$> readFile "in/09.txt"
  let cts = map (\d->ord d - ord '0') input
      orig = [ (if even j then j `div` 2 else -1, ct) | (j,ct) <- zip [0..] cts ]
      sized = M.fromListWith (++) [ (ct,[j]) | (j,ct) <- orig, j >= 0 ]
  print $ checksum 0 0 $ process sized S.empty orig -- part 2
