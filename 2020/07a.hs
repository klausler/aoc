import Data.List(isPrefixOf)
import Data.Maybe(isJust)
import qualified Data.Map as M
parseRec ln = ((adj ++ " " ++ color), f items)
  where
    (adj:color:"bags":"contain":items) = words ln
    f [] = []
    f ["no", "other", "bags."] = []
    f (ct:adj:color:b:rest)
      | "bag" `isPrefixOf` b = (read ct, adj ++ " " ++ color):f rest
search hits rest
  | M.null rest = hits
  | M.null more = hits
  | otherwise = search (hits `M.union` more) rest'
  where
    (more, rest') = M.partition f rest
    f = any (isJust . (`M.lookup` hits) . snd)
main = do
  txt <- readFile "input07.txt"
  let
    db = M.fromList $ map parseRec $ lines txt
    f = any ((=="shiny gold") . snd)
    (hits,rest) = M.partition f db
    ans = search hits rest
  print $ M.size ans
