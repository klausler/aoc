import Data.List(intercalate)
import qualified Data.Map as M
import qualified Data.Set as S
parse ln = (ingredients, map init $ tail t)
  where
    (ingredients, t) = break (=="(contains") $ words ln
f (ingredients, allergens) mIn = foldr g mIn allergens
  where
    iSet = S.fromList ingredients
    g a m = M.insertWith S.intersection a iSet m
solve soFar aMap
  | M.null aMap = soFar
  | otherwise = solve done $ M.map (S.\\ remove) rest
  where
    (new,rest) = M.partition ((==1) . S.size) aMap
    done = M.foldrWithKey M.insert soFar new
    remove = M.foldr S.union S.empty new
main = do
  txt <- readFile "in/21.txt"
  putStrLn $ intercalate "," $ map (head . S.toList . snd) $ M.assocs $ solve M.empty $ foldr f M.empty $ map parse $ lines txt
