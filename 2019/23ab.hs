-- import System.IO.Unsafe(unsafePerformIO)
import IntCode
import qualified Data.Map as M
packet (to:x:y:rest) = ([(to,[x,y])],rest)
packet o  = ([],o)
step nXnY state
  | isIdle, Just (x,y) <- nXnY = y : step nXnY' (state'' [x,y])
  | otherwise = step nXnY' state'
  where next = [ (j,((m',at',base'),i',o'',status,pkt))
               | (j,((m,at,base),i,o)) <- M.assocs state
               , let (m',at',base',i',o',status) = executeSingle m at base i
               , let (pkt,o'') = packet $ o++o' ]
        isIdle = and [ null i && null o && null pkt | (_,(_,i,o,status,pkt)) <- next ]
        traffic = M.fromListWith (flip (++)) $ concat [ pkt | (_,(_,_,_,_,pkt)) <- next ]
        inputs = [ M.findWithDefault [-1 | null i, status == NeedInput] j traffic
                 | (j,(_,i,_,status,_)) <- next ]
        state' = M.fromList [ (j,(s,i++i',o)) | ((j,(s,i,o,_,_)),i') <- zip next inputs ]
        state'' p0 | (s,i,[]) <- state' M.! 0 = M.insert 0 (s,i++p0,[]) state'
        nXnY' | Just xs <- M.lookup 255 traffic = let (y:x:_) = reverse xs in Just (x,y)
              | otherwise = nXnY
findDup (x:(rest@(y:z:_)))
  | x == y, y == z = x
  | otherwise = findDup rest
main = do
  m <- load <$> readFile "in/23.txt"
  let ys = step Nothing (M.fromList [ (j,((m,0,0),[j],[])) | j <- [0..49] ])
  print $ head ys -- part A
  print $ findDup ys -- part B
