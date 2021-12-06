import Data.List(tails,transpose,zipWith)
import qualified Data.Map as M
import qualified Data.Set as S
(sigFuncs@[topSig,bottomSig,leftSig,rightSig]) = [ head, last, map head, map last ]
imgSigs img = let sigs = map ($ img) sigFuncs in sigs ++ map reverse sigs
transforms = [id, reverse, map reverse, transpose, reverse.map reverse, reverse.transpose, map reverse.transpose, reverse.map reverse.transpose]
readTiles lns = ((read $ init $ drop 5 l1) :: Int, take 10 img) : readTiles lns'
  where
    (l1:img,lns') = splitAt 12 lns
findTile sigToTileSetMap sig remaining = S.toList $ (sigToTileSetMap M.! sig) `S.intersection` remaining
getRow tileMap sigToTileSetMap = row
  where
    row remaining sig
      | null tiles = (remaining, [])
      | otherwise = (remaining', img:imgs')
      where
        tiles = findTile sigToTileSetMap sig remaining
        img = head $ filter ((==sig).leftSig) $ map ($ (tileMap M.! head tiles)) transforms
        (remaining',imgs') = row (remaining `S.difference` S.fromList tiles) (rightSig img)
arrange tileMap sigToTileSetMap corner = doit remaining $ leftSig $ tileMap M.! corner
  where
    remaining = S.fromList $ M.keys tileMap
    doit remaining sig
      | S.null remaining = []
      | otherwise = row : doit remaining' nextSig
      where
        (remaining',row) = getRow tileMap sigToTileSetMap remaining sig
        nextStart = head $ findTile sigToTileSetMap (bottomSig $ head row) remaining'
        nextSig = leftSig $ head $ filter ((==(bottomSig $ head row)).topSig) $ map ($ (tileMap M.! nextStart)) transforms
crop = map (init.tail) . init . tail
mergeRow [x] = x
mergeRow (i:rest) = zipWith (++) i $ mergeRow rest
merge imgs = concatMap mergeRow imgs
isMonster (x:y:z:_)
  | length x < 20 = 0
  | pattern = 1 + rest
  | otherwise = rest
  where
    pattern = all (=='#') $ [x!!18] ++ map (y!!) [0, 5, 6, 11, 12, 17, 18, 19] ++ map (z!!) [1, 4, 7, 10, 13, 16]
    rest = isMonster $ map tail [x, y, z]
isMonster _ = 0
monsters xs = sum $ map isMonster $ tails xs
main = do
  txt <- readFile "input20.txt"
  let
    tiles = take 144 $ readTiles $ lines txt
    tileToSigSetMap = M.fromList [ (tn, S.fromList $ imgSigs img) | (tn,img) <- tiles ]
    sigToTileSetMap = foldr f M.empty [ (s, tn) | (tn,img) <- tiles, s <- imgSigs img ]
      where
        f (s,tn) m = M.insertWith S.union s (S.fromList [tn]) m
    neighbors tn = S.unions $ map (sigToTileSetMap M.!) $ S.toList $ tileToSigSetMap M.! tn
    corner = head [ tn | (tn,_) <- tiles, S.size (neighbors tn) == 3 ]
    arranged = arrange (M.fromList tiles) sigToTileSetMap corner
    merged = merge $ map (map crop) arranged
    xformed = head $ filter ((>0) . monsters) $ map ($ merged) transforms
    pounds = length $ filter (=='#') $ concat xformed
  print $ pounds - 15 * monsters xformed
