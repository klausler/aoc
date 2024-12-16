import qualified Data.Map as M
import qualified Data.Set as S
dirs=[(-1,0),(0,1),(1,0),(0,-1)]
astar board front done
  | S.null front = []
  | board M.! at == 'E' = (cost,path) : more
  | otherwise = more
  where
    ((cost,(at@(r,c)),dir,path),front') = S.deleteFindMin front
    done' = (at,dir) `S.insert` done
    nexts = [ (cost + (if dir' == dir then 1 else 1001), at', dir', at':path)
            | (dir'@(dr,dc))<-dirs, let at'=(r+dr,c+dc), at' `M.member` board
            , (at',dir') `S.notMember` done' ]
    more = astar board (front' `S.union` S.fromList nexts) done'
main = do
  lns <- lines <$> readFile "in/16.txt"
  let board = M.fromList[((r,c),ch)|(r,ln)<-zip [1..] lns, (c,ch)<-zip [1..] ln, ch /= '#']
      [start] = [rc|(rc,ch) <- M.assocs board, ch == 'S']
      front = S.singleton (0,start,(0,1),[start])
      solutions = astar board front S.empty
      best = fst $ head $ solutions
      visited = S.unions $ (S.fromList . snd) <$> takeWhile ((==best).fst) solutions
  print best -- part 1
  print $ S.size visited -- part 2
