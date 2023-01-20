import IntCode
import Data.Char(chr,ord)
import Data.List(isPrefixOf)
import qualified Data.Map as M
import qualified Data.Set as S
neighbors (r,c) = [ (r-1,c), (r,c+1), (r+1,c), (r,c-1) ]
left (-1,0) = (0,-1)
left (0,1) = (-1,0)
left (1,0) = (0,1)
left (0,-1) = (1,0)
walk scaffold (r,c) (dr,dc)
  | (r+dr,c+dc) `S.member` scaffold = 'F' : walk scaffold (r+dr,c+dc) (dr,dc)
  | (r+ldr,c+ldc) `S.member` scaffold = 'L' : walk scaffold (r,c) (ldr,ldc)
  | (r+rdr,c+rdc) `S.member` scaffold = 'R' : walk scaffold (r,c) (rdr,rdc)
  | otherwise = ""
  where (ldr,ldc) = left (dr,dc)
        (rdr,rdc) = left $ left (ldr,ldc)
squash n ('F':rest) = squash (n+1) rest
squash 0 "" = []
squash n "" = [show n]
squash 0 (x:xs) = [x] : squash 0 xs
squash n (x:xs) = show n : [x] : squash 0 xs
replace [] _ _ = []
replace rest pref ch
  | isPrefixOf pref rest = [ch] : replace (drop (length pref) rest) pref ch
  | otherwise = head rest : replace (tail rest) pref ch
splice [x] = x
splice (x:xs) = x ++ ',' : splice xs
done xs = all ((`elem` "ABC") . head) xs && length (splice xs) <= 20
reductions xs ch = [ (pref, replace xs pref ch) | n <- [1..length rest], let pref = take n rest, all ((`notElem` "ABC") . head) pref, length (splice pref) <= 20 ]
  where rest = dropWhile ((`elem` "ABC") . head) xs
main = do
  m <- load <$> readFile "in/17.txt"
  let view = lines $ chr <$> (snd $ run m 0 0 [])
      scaffold = S.fromList $ concat [ [ (r,c) | (c,ch) <- zip [0..] ln, ch /= '.' ] | (r,ln) <- zip [0..] view ]
      [('^',startRC)] = concat [ [ (ch,(r,c)) | (c,ch) <- zip [0..] ln, ch /= '#' && ch /= '.' ] | (r,ln) <- zip [0..] view ]
      isects = [ rc | rc <- S.toList scaffold, all (`S.member` scaffold) $ neighbors rc ]
      input = ord <$> (unlines $ [splice pMain, splice pA, splice pB, splice pC, "n"])
      tour = squash 0 $ walk scaffold startRC (-1,0)
      (pA,pB,pC,pMain) = head [ (pA, pB, pC, pMain)
                              | (pA,t') <- reductions tour 'A', (pB,t'') <- reductions t' 'B'
                              , (pC,pMain) <- reductions t'' 'C', done pMain ]
  print $ sum $ uncurry (*) <$> isects -- part A
  print $ last $ snd $ run (M.insert 0 2 m) 0 0 input -- part B
