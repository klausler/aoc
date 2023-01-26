import Data.List(elemIndex)
import qualified Data.Map as M
step deck ["cut",n'] = let (xs,ys) = splitAt amount deck in ys ++ xs
  where n = read n'
        amount = if n >= 0 then n else 10007+n
step deck ["deal","into",_,_] = reverse deck
step deck [_,_,_,n] = M.elems $ M.fromList $ zip ((`mod` 10007) <$> [0,read n..]) deck
main = do
  input <- ((words <$>) . lines) <$> readFile "in/22.txt"
  let final = foldl step [0..10006] input
  print $ elemIndex 2019 final
