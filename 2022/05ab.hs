import Data.List(transpose)
import qualified Data.Map as M
parse "" = []
parse (' ':' ':' ':' ':rest) = ' ' : parse rest
parse (' ':'[':ch:']':rest) = ch : parse rest
doit partFunc state (ct, from, to) = M.adjust (boxes ++) to $ M.adjust (drop ct) from state
  where boxes = partFunc $ take ct $ state M.! from
main = do
  input <- lines <$> readFile "in/05.txt"
  let (preamble, (_:rawMoves)) = break null input
      stacks = dropWhile (==' ') <$> transpose (parse <$> ((' ':) <$> init preamble))
      start = M.fromList $ zip [1..] stacks
      moves = [(read ct, read from, read to)
              | move <- rawMoves, let ["move", ct, "from", from, "to", to] = words move]
      finalA = foldl (doit reverse) start moves
      finalB = foldl (doit id) start moves
  putStrLn $ snd <$> M.toAscList finalA >>= take 1 -- part A
  putStrLn $ snd <$> M.toAscList finalB >>= take 1 -- part B
