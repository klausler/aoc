import Data.Bits
import Data.List(maximumBy)
import qualified Data.Map as M
rip ch = if ch >= '0' && ch <= '9' || ch >= 'A' && ch <= 'Z' then ch else ' '
parse ln map = let ("V":from:rate:to) = words $ rip <$> ln in M.insert from (read rate,to) map
links map0 = M.fromList [ (x,ys) | (x,(_,ys)) <- M.assocs map0 ]
flows map0 = M.fromList [ (x,xr) | (x,(xr,_)) <- M.assocs map0 ]
score path = score' 0 0 path
  where score' sc _ [] = sc
        score' sc incr ((_,i):rest) = score' (sc + incr) (incr + i) rest
pick a b = compare (score a) (score b)
visitedSets flows = vs' $ filter (>0) $ M.elems flows
  where vs' [] = [0]
        vs' (x:xs) = let v = vs' xs in v ++ (((bit x) .|.) <$> v)
startMap visitedSets map0 = M.fromList [ ((x,vs),[(x,0)]) | x <- M.keys map0, vs <- visitedSets ]
nextMap :: M.Map String [String] -> M.Map String Int -> [Int] -> M.Map (String,Int) [(String,Int)] -> M.Map (String,Int) [(String,Int)]
nextMap links flows visitedSets map = M.fromList [ ((x,vs), best x vs) | (x,vs) <- M.keys map ]
  where best x vs = maximumBy pick paths
          where xF = flows M.! x
                xF' = if vs `testBit` xF then 0 else xF
                vs' = vs `setBit` xF
                offPaths = [ (x,0) : (map M.! (y,vs)) | y <- links M.! x ]
                paths = if xF' > 0 then ((x,xF') : map M.! (x,vs')) : offPaths else offPaths
main = do
  map0 <- (foldr parse M.empty . lines) <$> readFile "in/16.txt"
  let fl = flows map0
      vs = visitedSets fl
      vals = iterate (nextMap (links map0) fl vs) (startMap vs map0)
      partB = vals !! 25
      allVs = foldl setBit 0 $ filter (>0) $ M.elems fl
  print $ score $ (vals !! 29) M.! ("AA", 0) -- part A
  print $ maximum [ score (partB M.! ("AA",v)) + score (partB M.! ("AA", allVs `xor` v)) | v <- vs ] -- part B
