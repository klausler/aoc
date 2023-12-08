import qualified Data.Map as M
import qualified Data.Set as S
-- Traverse the graph until a loop is found
travel graph set at ((lr,index):turns)
  | pt `S.member` set = [pt]
  | otherwise = pt : travel graph (S.insert pt set) (which $ graph M.! at) turns
  where
    pt = (at,index)
    which = if lr == 'L' then fst else snd
analyze trip
  -- Rely on input data actually having no prelude hit, just one hit per loop iteration,
  -- and the index of the first hit being equal to the loop length.
  | [] <- hits prelude, [loopHit] <- hits loop, length prelude + loopHit == magic = magic
  where
    (prelude,loop) = span (/=(last trip)) $ init trip
    hits pts = [ j | (j,(at,i)) <- zip [0..] pts, last at == 'Z' ]
    magic = length loop
main = do
  (turns:_:nodeLines) <- lines <$> readFile "in/08.txt"
  let graph = M.fromList [ (f,(init $ tail t1, init t2)) | [f,_,t1,t2] <- words <$> nodeLines ]
      starts = [ n | n <- M.keys graph, last n == 'A' ] :: [String]
      indexedTurns = cycle $ zip turns [0..]
      tours = [ travel graph S.empty s indexedTurns | s <- starts ]
  print $ foldr1 lcm $ map analyze tours -- part B
