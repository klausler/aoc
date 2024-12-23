import qualified Data.Map as M
numKeys = M.fromList
  [ ('7',(1,1)), ('8',(1,2)), ('9',(1,3)),
    ('4',(2,1)), ('5',(2,2)), ('6',(2,3)),
    ('1',(3,1)), ('2',(3,2)), ('3',(3,3)),
                 ('0',(4,2)), ('A',(4,3)) ]
dirKeys = M.fromList
  [              ('^',(1,2)), ('A',(1,3)),
    ('<',(2,1)), ('v',(2,2)), ('>',(2,3)) ]
times n x = take n $ repeat x
pathLDUR (fr,fc) (tr,tc) =
   (fc-tc) `times` '<' ++ (tr-fr) `times` 'v' ++
   (fr-tr) `times` '^' ++ (tc-fc) `times` '>' ++ "A"
pathUDLR (fr,fc) (tr,tc) =
   (fr-tr) `times` '^' ++ (tr-fr) `times` 'v' ++
   (fc-tc) `times` '<' ++ (tc-fc) `times` '>' ++ "A"
pathLRUD (fr,fc) (tr,tc) =
   (fc-tc) `times` '<' ++ (tc-fc) `times` '>' ++
   (fr-tr) `times` '^' ++ (tr-fr) `times` 'v' ++ "A"
numPath from to
  | fc == 1, tr == 4 = pathLRUD (fr,fc) (tr,tc)
  | fr == 4, tc == 1 = pathUDLR (fr,fc) (tr,tc)
  | otherwise = pathLDUR (fr,fc) (tr,tc)
  where [(fr,fc),(tr,tc)] = (numKeys M.!) <$> [from,to]
numDo 'A' [] = ""
numDo at (next:rest) = numPath at next ++ numDo next rest
dirPath from to
  | fc == 1, tr == 1 = pathLRUD (fr,fc) (tr,tc)
  | fr == 1, tc == 1 = pathUDLR (fr,fc) (tr,tc)
  | otherwise = pathLDUR (fr,fc) (tr,tc)
  where [(fr,fc),(tr,tc)] = (dirKeys M.!) <$> [from,to]
dirDo _ [] = ""
dirDo at (next:rest) = dirPath at next ++ dirDo next rest
transitions (dPath@(first:_)) = ('A':first:"") : tr dPath
  where tr (x:(rest@(y:_))) = (x:y:"") : tr rest
        tr _ = []
countTransitions path = M.fromListWith (+) $ [ (x,1) | x <- transitions path ]
expand (first:rest) = dirDo first rest
step m = M.fromListWith (+)
  [ (tr',ct * ct') | (tr,ct) <- M.assocs m
  , let m' = countTransitions $ expand tr
  , (tr',ct') <- M.assocs m' ]
trScore m = sum [ ct * (length $ expand tr) | (tr,ct) <- M.assocs m ]
solve n str = iterate step (countTransitions $ numDo 'A' str) !! n
score n str = (read $ init str) * (trScore $ solve n str)
main = do
  lns <- lines <$> readFile "in/21.txt"
  print $ sum $ score 1 <$> lns -- part 1
  print $ sum $ score 24 <$> lns -- part 2
