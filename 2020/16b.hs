import Control.Applicative((<*>))
import Data.List((\\),partition,isPrefixOf)
toRule r = (tag, \n -> (rf rng1 n || rf rng2 n))
  where
    (tag,':':r') = break (':'==) r
    [rng1,"or",rng2] = words r'
    rf rng = test
      where
        (a,'-':b) = break ('-'==) rng
        (a',b') = (read a, read b) :: (Int,Int)
        test n = n >= a' && n <= b'
vals = map read . words . map (\c -> if c == ',' then ' ' else c)
noRule rs n = not $ or $ map snd rs <*> [n]
canRuleBe vs r n = all (snd r) (map (!!n) vs)
solve done [] = done
solve done rest = solve ((tag,n):done) rest'''
  where
    (((tag,[n]):rest'), rest'') = partition ((==1).length.snd) rest
    rest''' = [ (tag,ns \\ [n]) | (tag,ns) <- rest' ++ rest'' ]
main = do
  txt <- readFile "in/16.txt"
  let
    (rules,"":"your ticket:":my:"":"nearby tickets:":nearby) = span (not.null) $ lines txt
    (allVals@(myVals:nearbyVals)) = vals my : map vals nearby
    rs = map toRule rules
    valid = filter (all (not . noRule rs)) allVals
    fields = [0..length myVals - 1]
  print $ product $ map ((myVals !!).snd) $ filter (("departure" `isPrefixOf`).fst) $
    solve [] [ (fst r, filter (canRuleBe valid r) fields) | r <- rs ]
