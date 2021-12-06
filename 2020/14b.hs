import Data.Char(isDigit)
import Data.List(isPrefixOf)
import qualified Data.Map as M
addrs [] a = [a]
addrs ('0':r) a = [ 2*x + (a `mod` 2) | x <- addrs r (a `div` 2) ]
addrs ('1':r) a = [ 2*x + 1 | x <- addrs r (a `div` 2) ]
addrs ('X':r) a = concat [[2*x,2*x+1] | x <- addrs r (a `div` 2) ]
step :: (String,M.Map Int Int) -> String -> (String,M.Map Int Int)
step (revMask,mem) ln
  | "mask = " `isPrefixOf` ln = (reverse $ drop 7 ln, mem)
  | "mem[" `isPrefixOf` ln = (revMask, M.union new mem)
  where
    (addr,rest) = span isDigit $ drop 4 ln
    (a,v) = (read addr, read $ drop 4 rest)
    new = M.fromList [ (x,v) | x <- addrs revMask a ]
main = do
  txt <- readFile "input14.txt"
  print $ sum $ M.elems $ snd $ foldl step (take 36 $ repeat 'X', M.empty) $ lines txt
