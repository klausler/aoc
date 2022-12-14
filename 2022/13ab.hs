import Data.Char(isDigit)
import Data.List(findIndices,sort)
data Type = Val Int | List [Type] deriving(Eq)
instance Ord Type where
  compare (Val x) (Val y) = compare x y
  compare (Val x) ys = compare (List [Val x]) ys
  compare (List []) (List []) = EQ
  compare (List []) (List _) = LT
  compare (List _) (List []) = GT
  compare (List (x:xs)) (List (y:ys)) = let c = compare x y in if c == EQ then compare (List xs) (List ys) else c
  compare xs (Val y) = compare xs (List [Val y])
pairs [] = []
pairs (x:y:rest) = (x,y) : pairs rest
getList str
  | ch == ',' = let (List xs, rest'') = getList rest' in (List (x:xs), rest'')
  | ch == ']' = (List [x], rest')
  where (x,(ch:rest')) = parse str
parse ('[':']':rest) = (List [], rest)
parse ('[':xs) = getList xs
parse str = let (digs, rest) = span isDigit str in (Val $ read digs, rest)
divider n = List [List [Val n]]
main = do
  parsed <- (((fst . parse) <$>) . filter (not.null) . lines) <$> readFile "in/13.txt"
  print $ sum [ j | (j,(lp,rp)) <- zip [1..] (pairs parsed), lp < rp ] -- part A
  let sorted = sort $ (divider 2 : divider 6 : parsed)
  print $ product $ (1+) <$> findIndices (\p->p == divider 2 || p == divider 6) sorted -- part B