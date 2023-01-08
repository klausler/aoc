import qualified Data.Set as S
import qualified Data.Map as M
halo (r,c) = [(r+dr,c+dc) | dr <- [-1..1], dc <- [-1..1], dr /= 0 || dc /= 0 ]
dirs = [(\(r,c)->[(r-1,c+dc) | dc<-[0,-1,1]]), -- N
        (\(r,c)->[(r+1,c+dc) | dc<-[0,-1,1]]), -- S
        (\(r,c)->[(r+dr,c-1) | dr<-[0,-1,1]]), -- W
        (\(r,c)->[(r+dr,c+1) | dr<-[0,-1,1]])] -- E
chunks = cycle [ take 4 $ drop n $ cycle dirs | n <- [0..3] ]
doRound elves dirs = S.unions [ stay, moved, cantMove ]
  where stay = S.fromList $ filter (S.null . S.intersection elves . S.fromList . halo) $ S.toList elves
        mightMove = elves S.\\ stay
        moves (r,c) = take 1 [ head side | side <- dirs <*> [(r,c)], S.null $ S.intersection elves $ S.fromList side ]
        destCnts = foldl accum M.empty $ S.toList mightMove >>= moves
        accum m rc = M.insertWith (+) rc 1 m
        canMove = S.fromList [ (r,c) | (r,c) <- S.toList mightMove, to <- take 1 $ moves (r,c), destCnts M.! to == 1 ]
        moved = S.fromList $ (head . moves) <$> S.toList canMove
        cantMove = mightMove S.\\ canMove
partB n elves (dirs:rest)
  | elves' == elves = n
  | otherwise = partB (n+1) elves' rest
  where elves' = doRound elves dirs
main = do
  input <- lines <$> readFile "in/23.txt"
  let elves = S.fromList [ (r,c) | (r,ln) <- zip [0..] input, (c,ch) <- zip [0..] ln, ch == '#' ]
      (rs,cs) = unzip $ S.toList $ foldl doRound elves $ take 10 chunks
  print $ (maximum rs - minimum rs + 1) * (maximum cs - minimum cs + 1) - S.size elves -- part A
  print $ partB 1 elves chunks -- part B
