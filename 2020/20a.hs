import qualified Data.Map as M
import qualified Data.Set as S
readTiles lns = ((read $ init $ drop 5 l1) :: Int, take 10 img) : readTiles lns'
  where
    (l1:img,lns') = splitAt 12 lns
imgSig img = map ($ img) [ head, last, map head, map last ]
main = do
  txt <- readFile "input20.txt"
  let
    tiles = take 144 $ readTiles $ lines txt
    tileToSigSetMap = M.fromList [ (tn, S.fromList (sigs ++ map reverse sigs)) | (tn,img) <- tiles, let sigs = imgSig img ]
    sigToTileSetMap = foldr f M.empty [ (s, tn) | (tn,img) <- tiles, let sig = imgSig img, s <- sig ++ map reverse sig ]
      where
        f (s,tn) m = M.insertWith S.union s (S.fromList [tn]) m
    neighbors tn = S.unions $ map (sigToTileSetMap M.!) $ S.toList $ tileToSigSetMap M.! tn
  print $ product [ tn | (tn,_) <- tiles, S.size (neighbors tn) == 3 ]
