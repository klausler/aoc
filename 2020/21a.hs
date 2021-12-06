import qualified Data.Map as M
import qualified Data.Set as S
parse :: String -> ([String],[String])
parse ln = (ingredients, map init $ tail t)
  where
    (ingredients, t) = break (=="(contains") $ words ln
f :: ([String],[String]) -> M.Map String (S.Set String) -> M.Map String (S.Set String)
f (ingredients, allergens) mIn = foldr g mIn allergens
  where
    iSet = S.fromList ingredients
    g a m = M.insertWith S.intersection a iSet m
main = do
  txt <- readFile "input21.txt"
  let
    rules = map parse $ lines txt
    aMap = foldr f M.empty rules
    dangerous = foldr S.union S.empty $ M.elems aMap
    ingredients = S.fromList $ rules >>= fst
    safe = ingredients S.\\ dangerous
  print $ length $ rules >>= (filter (`S.member` safe) . fst)
