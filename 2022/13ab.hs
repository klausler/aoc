import Data.Char(isDigit)
import Data.List(findIndices,sortBy)
data Type = Val Int | List [Type] deriving(Eq)
pairs [] = []
pairs (x:y:rest) = (x,y) : pairs rest
getList str
  | ch == ',' = let (List xs, rest'') = getList rest' in (List (x:xs), rest'')
  | ch == ']' = (List [x], rest')
  where (x,(ch:rest')) = parse str
parse ('[':']':rest) = (List [], rest)
parse ('[':xs) = getList xs
parse str = let (digs, rest) = span isDigit str in (Val $ read digs, rest)
cmp (Val x) (Val y) = compare x y
cmp (Val x) ys = cmp (List [Val x]) ys
cmp (List []) (List []) = EQ
cmp (List []) (List _) = LT
cmp (List _) (List []) = GT
cmp (List (x:xs)) (List (y:ys)) = let c = cmp x y in if c == EQ then cmp (List xs) (List ys) else c
cmp xs (Val y) = cmp xs (List [Val y])
divider n = List [List [Val n]]
main = do
  parsed <- (((fst . parse) <$>) . filter (not.null) . lines) <$> readFile "in/13.txt"
  print $ sum [ j | (j,(lp,rp)) <- zip [1..] (pairs parsed), cmp lp rp == LT ] -- part A
  let sorted = sortBy cmp $ (divider 2 : divider 6 : parsed)
  print $ product $ (1+) <$> findIndices (\p->p == divider 2 || p == divider 6) sorted -- part B