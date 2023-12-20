import Data.Char(isDigit)
import qualified Data.Map as M
chunked [] = []
chunked ("":rest) = chunked rest
chunked lns = let (chunk, rest) = break null lns in chunk : chunked rest
data Step = Carve Int Char Int String | Final String
parseStep str
  | (xmas:ltgt:_) <- str, ltgt == '<' || ltgt == '>' = Carve (which xmas) ltgt (read ns) to
  | otherwise = Final str
  where
    (ns,(':':to)) = span isDigit $ drop 2 str
    which 'x' = 0; which 'm' = 1; which 'a' = 2; which 's' = 3
parseRule ln = (name, parseStep <$> steps)
  where (name:steps) = words $ (\ch -> if ch=='{' || ch == ',' then ' ' else ch) <$> init ln
sizeI (lo,hi) = hi - lo + 1
intersectI (xlo,xhi) (ylo,yhi) = (max xlo ylo, min xhi yhi)
intersectIS _ [] = []
intersectIS [] _ = []
intersectIS (xs@((x@(xlo,xhi)):xs')) (ys@((y@(ylo,yhi)):ys'))
  | xhi < ylo = intersectIS xs' ys
  | yhi < xlo = intersectIS xs ys'
  | otherwise = intersectI x y : intersectIS ([(yhi+1,xhi)|xhi>yhi]++xs') ([(xhi+1,yhi)|yhi>xhi]++ys')
intersectV = zipWith intersectIS
universeV = [[(1,4000)],[(1,4000)],[(1,4000)],[(1,4000)]]
volumeMasks w '<' n = (take w universeV ++ ([(1,n-1)] : drop (w+1) universeV), take w universeV ++ ([(n,4000)] : drop (w+1) universeV))
volumeMasks w '>' n = (take w universeV ++ ([(n+1,4000)] : drop (w+1) universeV), take w universeV ++ ([(1,n)] : drop (w+1) universeV))
analyzeRule fullMap start (name,steps) map = M.insert name (analyzeSteps steps start) map
  where
    analyzeSteps ((Final next):_) mask = intersectV mask <$> (fullMap M.! next)
    analyzeSteps ((Carve xmas ltgt n next):rest) mask = vs ++ following
      where
        (thisVol,thisVol') = volumeMasks xmas ltgt n
        vs = intersectV (thisVol `intersectV` mask) <$> (fullMap M.! next)
        mask' = mask `intersectV` thisVol'
        following = analyzeSteps rest mask'
main = do
  [ruleLns,_] <- (chunked.lines) <$> readFile "in/19.txt"
  let rules = map parseRule ruleLns
      map0 = M.fromList [("R", []), ("A",[universeV])]
      analyzedMap = foldr (analyzeRule analyzedMap universeV) map0 rules
  print $ sum $ map (product . map (sum . map sizeI)) $ analyzedMap M.! "in"
