import Data.Char(isDigit)
import Data.List(isPrefixOf)
import qualified Data.Map as M
apply [] n = n
apply ('X':r) n = 2 * apply r (n `div` 2) + (n `mod` 2)
apply ('0':r) n = 2 * apply r (n `div` 2) + 0
apply ('1':r) n = 2 * apply r (n `div` 2) + 1
step :: (String,M.Map Int Int) -> String -> (String,M.Map Int Int)
step (revMask,mem) ln
  | "mask = " `isPrefixOf` ln = (reverse $ drop 7 ln, mem)
  | "mem[" `isPrefixOf` ln = (revMask, M.insert a (apply revMask v) mem)
  where
    (addr,rest) = span isDigit $ drop 4 ln
    (a,v) = (read addr, read $ drop 4 rest)
main = do
  txt <- readFile "input14.txt"
  print $ sum $ M.elems $ snd $ foldl step (take 36 $ repeat 'X', M.empty) $ lines txt
