import Data.List(sort,maximumBy)
import qualified Data.Map as M
noteSleep m g from to = M.insert g (note $ M.findWithDefault M.empty g m) m
  where note gm = foldr incr gm [from..to-1]
        incr t gm = M.insertWith (+) t 1 gm
process m (Just g) (Just min) [] = noteSleep m g min 60
process m _ _ [] = m
process m g asleep (ln:rest)
  | ch == 'G', Just g' <- g, Just min' <- asleep = process (noteSleep m g' min' 60) (Just guard) Nothing rest
  | ch == 'G' = process m (Just guard) Nothing rest
  | ch == 'f', Just _ <- g, Nothing <- asleep = process m g (Just min) rest
  | ch == 'w', Just g' <- g, Just min' <- asleep = process (noteSleep m g' min' min) g Nothing rest
  where ch = head $ drop 19 ln
        min = read $ take 2 $ drop 15 ln
        guard = read $ head $ words $ drop 26 ln
((.*.) `on` f) x y = f x .*. f y
worst = fst . maximumBy (compare `on` snd)
main = do
  input <- lines <$> readFile "in/04.txt"
  let naps = process M.empty Nothing Nothing $ sort input
      sleepiestG = worst [ (g, sum $ M.elems gm) | (g, gm) <- M.assocs naps ]
  print $ sleepiestG * (worst $ M.assocs $ naps M.! sleepiestG) -- part A
  print $ uncurry (*) $ worst $ [ ((g,min),total)
                                | (g,gm) <- M.assocs naps, (min,total) <- M.assocs gm ] -- part B
