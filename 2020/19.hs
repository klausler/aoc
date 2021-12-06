import qualified Data.Map as M
chop _ [] = []
chop f xs = let (ys,rest) = break f xs in if null rest then [ys] else ys : (chop f $ tail rest)
chain [] txt = [txt]
chain (f:fs) txt = f txt >>= chain fs
compile ruleMap s = (read $ init rnum, toFunc rest)
  where
    (rnum:rest) = words s
    toFunc [['"', ch, '"']] str = if [ch] == take 1 str then [tail str] else []
    toFunc xs str = fns >>= ($ str)
      where
        fns = map (chain . map ((ruleMap M.!).read)) $ chop (=="|") xs
main = do
  txt <- readFile "in/19b.txt" -- use input19a.txt for part one
  let
    (rules,("":cases)) = break null $ lines txt
    ruleMap = M.fromList $ map (compile ruleMap) rules
    isMatch = any null . (ruleMap M.! 0)
  print $ length $ filter isMatch cases
