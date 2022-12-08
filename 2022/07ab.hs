import qualified Data.Map as M
data Item = File Int | Dir (M.Map String Item)
addFile fsz [x] tree = M.insert x (File fsz) tree
addFile fsz (x:xs) tree = M.insert x (Dir (addFile fsz xs subt)) tree
  where Dir subt = M.findWithDefault (Dir M.empty) x tree
step tree _ [] = tree
step tree path (["$", "cd", "/"]:rest) = step tree ["/"] rest
step tree path (["$", "cd", ".."]:rest) = step tree (init path) rest
step tree path (["$", "cd", dir]:rest) = step tree (path++[dir]) rest
step tree path (("$":_):rest) = step tree path rest -- "ls"
step tree path (("dir":_):rest) = step tree path rest -- ignored
step tree path ([sz, f]:rest) = step (addFile (read sz) (path++[f]) tree) path rest
walk tree = (sum smallSzs + (if totalSz <= 100000 then totalSz else 0), totalSz, totalSz : concat allSzs)
  where (smallSzs, allSzSums, allSzs) = unzip3 [ walk t | Dir t <- M.elems tree ]
        totalSz = sum [ fsz | File fsz <- M.elems tree ] + sum allSzSums
main = do
  input <- (words <$>) <$> (lines <$> readFile "in/07.txt")
  let tree = step M.empty ["/"] input
      (smallSzSum, totalSz, sizes) = walk tree
  print smallSzSum -- part A
  print $ minimum $ filter (>=(30000000 - (70000000 - totalSz))) sizes -- partB
