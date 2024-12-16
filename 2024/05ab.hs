import qualified Data.Map as M
import qualified Data.Set as S
import Data.List(elemIndex,partition)
import Data.Maybe(isJust)
getRule ln
  | Just n <- elemIndex '|' ln, (a,(_:b)) <- splitAt n ln = [(read a, read b)]
  | otherwise = []
getPages ln
  | isJust $ elemIndex ',' ln = [read $ '[':ln ++ "]"]
  | otherwise = []
isValidPageList :: M.Map Int (S.Set Int) -> [Int] -> Bool
isValidPageList depMap pages = isValid S.empty pages
  where
    allPages = S.fromList pages
    isValid _ [] = True
    isValid soFar (pg:pgs)
      | need `S.isSubsetOf` soFar = isValid (S.insert pg soFar) pgs
      | otherwise = False
      where need = M.findWithDefault S.empty pg depMap `S.intersection` allPages
topological depMap pages = order [] S.empty allPages
  where
    allPages = S.fromList pages
    order path soFar pgs
      | S.null pgs = reverse path
      | otherwise = order (next:path) (S.insert next soFar) (S.delete next pgs)
      where
        (next:_) = filter canPrint $ S.toList pgs
        canPrint pg = (M.findWithDefault S.empty pg depMap `S.intersection` allPages) `S.isSubsetOf` soFar
getMiddle xs = xs !! (length xs `div` 2)
main = do
  lns <- lines <$> readFile "in/05.txt"
  let depMap = M.fromListWith S.union [ (b, S.singleton a) | (a,b) <- lns >>= getRule ]
      (valid, invalid) = partition (isValidPageList depMap) $ lns >>= getPages
  print $ sum $ getMiddle <$> valid -- part 1
  print $ sum $ (getMiddle . topological depMap) <$> invalid -- part 2
